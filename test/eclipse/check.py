#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Launch the Eclipse SDK under Xvfb and exercise a real Java editing/build session."""
import argparse
import contextlib
import hashlib
import json
import os
from pathlib import Path
import select
import shlex
import shutil
import subprocess
import tarfile
import tempfile
import time
import urllib.request

ROOT = Path(__file__).resolve().parents[2]
BUILD = ROOT / '.cache/eclipse'
SOURCE = ROOT / 'test/eclipse'
JDK = Path(os.environ['JAVA_HOME'])
URL = ('https://archive.eclipse.org/eclipse/downloads/drops4/R-4.41-202608281142/'
       'eclipse-SDK-4.41-linux-gtk-x86_64.tar.gz')
SHA512 = ('503f757a096256f719139db67e2537478542585844876e0d68f702ca51dbc1a0282d3'
          'a5fa5023d77c1013c1b35a90525cc02aeed1af17ced26bde293da46d2cf')
MARKER = 'PASS: workbench, Java editor, save, error diagnostics, corrected build'


def prepare():
    BUILD.mkdir(parents=True, exist_ok=True)
    archive = BUILD / 'eclipse-sdk.tar.gz'
    if not archive.exists():
        temporary = archive.with_suffix('.part')
        with urllib.request.urlopen(URL, timeout=60) as source, temporary.open('wb') as target:
            shutil.copyfileobj(source, target)
        temporary.replace(archive)
    with archive.open('rb') as stream:
        if hashlib.file_digest(stream, 'sha512').hexdigest() != SHA512:
            raise RuntimeError('Eclipse SDK checksum mismatch')
    install = BUILD / 'eclipse'
    if not install.exists():
        with tarfile.open(archive) as source:
            source.extractall(BUILD, filter='data')
    bundle = BUILD / 'check-smoke-bundle'
    bundle.mkdir(exist_ok=True)
    classpath = os.pathsep.join(str(path) for path in (install / 'plugins').glob('*.jar'))
    subprocess.run([str(JDK / 'bin/javac'), '--release', '21', '-cp', classpath,
                    '-d', str(bundle), str(SOURCE / 'SmokeTest.java')], check=True, timeout=60)
    jar = BUILD / 'check-smoke-test.jar'
    subprocess.run([str(JDK / 'bin/jar'), '--create', '--file', str(jar), '--manifest',
                    str(SOURCE / 'MANIFEST.MF'), '-C', str(bundle), '.',
                    '-C', str(SOURCE), 'plugin.xml'], check=True, timeout=60)
    return install, jar


def stop(process):
    if process is not None and process.poll() is None:
        process.terminate()
        try:
            process.wait(timeout=10)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()


@contextlib.contextmanager
def display(run):
    read_fd, write_fd = os.pipe()
    server = None
    try:
        with (run / 'xvfb.log').open('w') as log:
            server = subprocess.Popen(shlex.split(os.environ.get('XVFB', 'Xvfb'))
                + ['-displayfd', str(write_fd), '-screen', '0', '1024x768x24', '-ac', '-nolisten', 'tcp'],
                pass_fds=(write_fd,), stdout=log, stderr=subprocess.STDOUT)
        os.close(write_fd)
        write_fd = None
        if not select.select([read_fd], [], [], 20)[0]:
            raise RuntimeError(f'Xvfb startup timeout; see {run / "xvfb.log"}')
        number = os.read(read_fd, 64).decode().strip()
        if not number.isdigit():
            raise RuntimeError(f'Xvfb failed; see {run / "xvfb.log"}')
        yield dict(os.environ, DISPLAY=':' + number)
    finally:
        os.close(read_fd)
        if write_fd is not None:
            os.close(write_fd)
        stop(server)


def check(name, engine, install, bundle, timeout, runtime):
    runs = BUILD / 'runs'
    runs.mkdir(exist_ok=True)
    run = Path(tempfile.mkdtemp(prefix=name + '-', dir=runs))
    configuration = run / 'configuration'
    shutil.copytree(install / 'configuration', configuration)
    info = configuration / 'org.eclipse.equinox.simpleconfigurator/bundles.info'
    info.write_text(info.read_text() + f'\norg.openldk.eclipse.test,1.0.0,{bundle.as_uri()},4,false\n')
    result = run / 'result.txt'
    close = run / 'close-request'
    workspace = run / 'workspace'
    launcher = next((install / 'plugins').glob('org.eclipse.equinox.launcher_*.jar'))
    command = engine + [f'-Dopenldk.eclipse.result={result}', f'-Dopenldk.eclipse.close={close}',
                        '-jar', str(launcher), '-nosplash', '-consoleLog',
                        '-install', str(install), '-configuration', str(configuration),
                        '-data', str(workspace)]
    (run / 'command.txt').write_text(shlex.join(command) + '\n')
    print(f'{name}: starting Eclipse; logs in {run}', flush=True)
    with display(run) as env, (run / 'console.log').open('w') as log:
        process = subprocess.Popen(command, cwd=install, env=env, stdout=log, stderr=subprocess.STDOUT)
        try:
            deadline = time.monotonic() + timeout
            while not result.exists():
                if process.poll() is not None:
                    raise RuntimeError(f'{name}: Eclipse exited {process.returncode}; see {run}')
                if time.monotonic() >= deadline:
                    raise RuntimeError(f'{name}: Eclipse smoke test timed out; see {run}')
                time.sleep(0.5)
            report = result.read_text()
            if not report.startswith(MARKER):
                raise RuntimeError(f'{name}: {report}\nLogs: {run}')
            if name == 'openldk':
                maps = Path(f'/proc/{process.pid}/maps').read_text()
                libraries = sorted({line.split()[-1] for line in maps.splitlines()
                                    if '.so' in line and line.split()[-1].startswith('/')})
                (run / 'native-libraries.txt').write_text('\n'.join(libraries) + '\n')
                if any(path.startswith(str(JDK.resolve()) + '/') for path in libraries):
                    raise RuntimeError(f'OpenLDK loaded a JDK native library; see {run / "native-libraries.txt"}')
                if not any('/libswt-' in path for path in libraries):
                    raise RuntimeError(f'OpenLDK did not load the SWT JNI libraries; see {run}')
            output = subprocess.run([str(runtime.resolve()), '-cp',
                                     str(workspace / 'OpenLDKSmoke/bin'), 'Hello'], cwd=ROOT,
                                    env=env, capture_output=True, text=True, timeout=60)
            (run / 'hello.log').write_text(output.stdout + output.stderr)
            if output.returncode or output.stdout.strip() != 'Hello from Eclipse on OpenLDK':
                raise RuntimeError(f'{name}: compiled program failed; see {run / "hello.log"}')
            subprocess.run(['python3', str(SOURCE / 'capture.py'), str(run / 'workbench.png')],
                           env=env, check=True, timeout=30)
            close.touch()
            if process.wait(timeout=120) != 0:
                raise RuntimeError(f'{name}: Eclipse shutdown failed; see {run}')
        finally:
            stop(process)
    print(f'{name}: editor, diagnostics, build, compiled program, screenshot, and clean shutdown passed', flush=True)
    return str(run)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--engine', choices=['both', 'hotspot', 'openldk'], default='both')
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    parser.add_argument('--timeout', type=int, default=1800)
    options = parser.parse_args()
    install, bundle = prepare()
    latest = BUILD / 'latest.json'
    results = json.loads(latest.read_text()) if latest.exists() else {}
    engines = [('hotspot', [str(JDK / 'bin/java'), '--enable-native-access=ALL-UNNAMED', '-Xmx2g']),
               ('openldk', [str(options.runtime.resolve())])]
    for name, engine in engines:
        if options.engine not in ['both', name]:
            continue
        results[name] = check(name, engine, install, bundle, options.timeout, options.runtime)
        latest.write_text(json.dumps(results, indent=2) + '\n')


if __name__ == '__main__':
    main()
