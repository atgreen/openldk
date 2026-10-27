#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Run Eclipse on OpenLDK, optionally installing the pinned Eclipse SDK first."""
import argparse
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys
import tempfile

from sdk import CACHE, DEFAULT_INSTALL, ROOT, ensure_sdk


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--download', action='store_true', help='download and install Eclipse if missing')
    parser.add_argument('--install-dir', type=Path, default=DEFAULT_INSTALL, help='Eclipse installation directory')
    parser.add_argument('--install-only', action='store_true', help='ensure Eclipse is installed, then exit')
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk', help='OpenLDK executable')
    parser.add_argument('--workspace', type=Path, help='desktop workspace (default: .cache/eclipse/workspace)')
    parser.add_argument('--smoke-test', '--headless', action='store_true',
                        help='run the automated editor/save/build check under Xvfb')
    parser.add_argument('--timeout', type=int, default=3600, help='smoke-test timeout in seconds (default: 3600)')
    parser.add_argument('--dry-run', action='store_true', help='print the desktop launch command without running it')
    options = parser.parse_args()
    if sys.version_info < (3, 12):
        parser.error('Python 3.12 or newer is required')
    if options.smoke_test and (options.workspace or options.dry_run or options.install_only):
        parser.error('--smoke-test uses its own fresh workspace; omit --workspace, --dry-run, and --install-only')
    if options.timeout <= 0:
        parser.error('--timeout must be positive')
    try:
        install = ensure_sdk(options.install_dir, download=options.download)
        if options.install_only:
            print(f'Eclipse: {install}')
            return 0
        java_home = os.environ.get('JAVA_HOME')
        java_home = Path(java_home).expanduser().resolve() if java_home else None
        if not java_home or not (java_home / 'bin/javac').is_file():
            parser.error('Set JAVA_HOME to a full JDK 25 installation')
        if not re.search(r'^JAVA_VERSION="25(?:[.+-]|")', (java_home / 'release').read_text(), re.MULTILINE):
            parser.error('JAVA_HOME must point to JDK 25')
        os.environ['JAVA_HOME'] = str(java_home)
        runtime = options.runtime.expanduser().resolve()
        if not runtime.is_file() or not os.access(runtime, os.X_OK):
            parser.error(f'OpenLDK executable missing: {runtime}; build it with make openldk jni')
        bridge = Path(os.environ.get('OPENLDK_JNI_BRIDGE', ROOT / 'libopenldk-jni.so')).expanduser().resolve()
        if not bridge.is_file():
            parser.error(f'JNI bridge missing: {bridge}; build it with make jni')
        os.environ['OPENLDK_JNI_BRIDGE'] = str(bridge)
        # Import after checking JAVA_HOME: the existing test harness uses it.
        from check import check, check_svg, prepare, stop
        if options.smoke_test:
            CACHE.mkdir(parents=True, exist_ok=True)
            build = Path(tempfile.mkdtemp(prefix='local-smoke-', dir=CACHE))
            print(f'Smoke-test artifacts: {build}', flush=True)
            install, bundle = prepare(install, download=False, build=build)
            check_svg('openldk', [str(runtime)], install, build=build)
            run = check('openldk', [str(runtime)], install, bundle, options.timeout, runtime, build=build)
            (build / 'latest.json').write_text(json.dumps({'openldk': run}, indent=2) + '\n')
            return 0
        if not options.dry_run and not (os.environ.get('DISPLAY') or os.environ.get('WAYLAND_DISPLAY')):
            parser.error('No desktop display; use --smoke-test for a headless Xvfb run')
        workspace = (options.workspace or CACHE / 'workspace').expanduser().resolve()
        launches = CACHE / 'launches'
        if options.dry_run:
            run = launches / 'preview'
        else:
            launches.mkdir(parents=True, exist_ok=True)
            run = Path(tempfile.mkdtemp(prefix='openldk-', dir=launches))
            shutil.copytree(install / 'configuration', run / 'configuration')
            workspace.mkdir(parents=True, exist_ok=True)
        launcher = next((install / 'plugins').glob('org.eclipse.equinox.launcher_*.jar'))
        command = [str(runtime), '-Djava.awt.headless=true', '-jar', str(launcher),
                   '-nosplash', '-consoleLog', '-install', str(install),
                   '-configuration', str(run / 'configuration'), '-data', str(workspace)]
        print(shlex.join(command), flush=True)
        if options.dry_run:
            return 0
        (run / 'command.txt').write_text(shlex.join(command) + '\n')
        print(f'Workspace: {workspace}\nConsole log: {run / "console.log"}', flush=True)
        print('Cold startup can take 15 minutes or longer while Java methods compile.', flush=True)
        process = None
        try:
            with (run / 'console.log').open('w') as log:
                process = subprocess.Popen(command, cwd=install, stdout=subprocess.PIPE,
                                           stderr=subprocess.STDOUT, text=True, errors='replace')
                for line in process.stdout:
                    log.write(line)
                    log.flush()
                    print(line, end='', flush=True)
                return process.wait()
        except KeyboardInterrupt:
            return 130
        finally:
            stop(process)
    except (OSError, RuntimeError, subprocess.SubprocessError) as error:
        parser.exit(1, f'{error}\n')


if __name__ == '__main__':
    sys.exit(main())
