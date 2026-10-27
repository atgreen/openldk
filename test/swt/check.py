#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Run the same SWT window test on HotSpot and OpenLDK under Xvfb."""
import hashlib
import os
from pathlib import Path
import select
import shlex
import subprocess
import urllib.request
import zipfile

ROOT = Path(__file__).resolve().parents[2]
BUILD = ROOT / '.cache/swt'
JDK = Path(os.environ['JAVA_HOME'])
VERSION = '3.135.0'
SHA256 = '0799e4475b8937e05a0f13c6cbeadbf066ca56dd3622bfd8c7a4ba133b2db2e2'
MARKER = 'SWT window, text widget, timer callback: PASS'


def main():
    BUILD.mkdir(parents=True, exist_ok=True)
    jar = BUILD / 'swt.jar'
    if not jar.exists():
        artifact = 'org.eclipse.swt.gtk.linux.x86_64'
        urllib.request.urlretrieve(
            f'https://repo.maven.apache.org/maven2/org/eclipse/platform/'
            f'{artifact}/{VERSION}/{artifact}-{VERSION}.jar', jar)
    if hashlib.sha256(jar.read_bytes()).hexdigest() != SHA256:
        raise RuntimeError('SWT checksum mismatch')
    native = BUILD / 'native'
    native.mkdir(exist_ok=True)
    with zipfile.ZipFile(jar) as archive:
        for name in archive.namelist():
            if name.endswith('.so'):
                (native / Path(name).name).write_bytes(archive.read(name))
    subprocess.run([str(JDK / 'bin/javac'), '-cp', str(jar), '-d', str(BUILD),
                    str(ROOT / 'test/swt/SWTProbe.java')], check=True, timeout=60)
    # -displayfd lets Xvfb choose an unused display and signals readiness.
    read_fd, write_fd = os.pipe()
    with (BUILD / 'xvfb.log').open('w') as log:
        try:
            server = subprocess.Popen(
                shlex.split(os.environ.get('XVFB', 'Xvfb'))
                + ['-displayfd', str(write_fd), '-screen', '0', '800x600x24',
                   '-ac', '-nolisten', 'tcp'],
                pass_fds=(write_fd,), stdout=log, stderr=subprocess.STDOUT)
        finally:
            os.close(write_fd)
        try:
            if not select.select([read_fd], [], [], 20)[0]:
                raise RuntimeError(f'Xvfb did not start; see {BUILD / "xvfb.log"}')
            display = os.read(read_fd, 64).decode().strip()
            if not display.isdigit():
                raise RuntimeError(f'Xvfb failed; see {BUILD / "xvfb.log"}')
            env = dict(os.environ, DISPLAY=':' + display)
            engines = [('hotspot', [str(JDK / 'bin/java'), '--enable-native-access=ALL-UNNAMED']),
                       ('openldk', [str(ROOT / 'openldk')])]
            for name, engine in engines:
                result = subprocess.run(
                    engine + [f'-Djava.library.path={native}', '-cp',
                              f'{jar}{os.pathsep}{BUILD}', 'SWTProbe'],
                    cwd=ROOT, env=env, stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT, text=True, timeout=180)
                (BUILD / f'{name}.log').write_text(result.stdout)
                if result.returncode or MARKER not in result.stdout or 'SWT PNG file image: PASS' not in result.stdout:
                    raise RuntimeError(f'{name} failed:\n{result.stdout}')
                print(f'{name}: {MARKER}', flush=True)
        finally:
            os.close(read_fd)
            server.terminate()
            try:
                server.wait(timeout=10)
            except subprocess.TimeoutExpired:
                server.kill()
                server.wait()


if __name__ == '__main__':
    main()
