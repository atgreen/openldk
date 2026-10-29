#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Compare defining-loader retention and collection with JDK 25."""
import argparse
import os
from pathlib import Path
import subprocess
import shlex

ROOT = Path(__file__).resolve().parents[2]
JDK = Path(os.environ['JAVA_HOME'])
BUILD = ROOT / '.cache/unloading-test'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--reference-only', action='store_true')
    parser.add_argument('--roots-only', action='store_true')
    parser.add_argument('--jni', action='store_true')
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    args = parser.parse_args()
    payload = BUILD / 'payload'
    app = BUILD / 'app'
    payload.mkdir(parents=True, exist_ok=True)
    app.mkdir(parents=True, exist_ok=True)
    subprocess.run([str(JDK / 'bin/javac'), '--release', '25', '-g', '-d', str(payload)]
                   + [str(path) for path in sorted((ROOT / 'test/unloading/payload').glob('*.java'))], check=True, timeout=60)
    subprocess.run([str(JDK / 'bin/javac'), '--release', '25', '-g', '-d', str(app),
                    str(ROOT / 'test/unloading/UnloadingProbe.java'), str(ROOT / 'test/unloading/UnloadingJNIProbe.java')], check=True, timeout=60)
    library = BUILD / 'libunloading.so'
    if args.jni:
        subprocess.run(shlex.split(os.environ.get('CC', 'cc')) + ['-Wall', '-Wextra', '-Werror', '-shared', '-fPIC',
                        f'-I{JDK}/include', f'-I{JDK}/include/linux', str(ROOT / 'test/unloading/probe.c'), '-o', str(library)], check=True, timeout=60)
    engines = [JDK / 'bin/java'] + ([] if args.reference_only else [args.runtime.resolve()])
    for engine in engines:
        environment = os.environ.copy()
        if args.jni:
            log = BUILD / f'{engine.name}-events.log'
            log.unlink(missing_ok=True)
            environment['OPENLDK_UNLOAD_LOG'] = str(log)
            command = [str(engine), '--enable-native-access=ALL-UNNAMED', '-cp', str(app), 'UnloadingJNIProbe', str(payload), str(library)]
        else:
            command = [str(engine), '-cp', str(app), 'UnloadingProbe', str(payload)]
        if args.roots_only:
            command.append('roots-only')
        result = subprocess.run(command, env=environment, capture_output=True, text=True, timeout=240)
        output = result.stdout + result.stderr
        suffix = '-jni' if args.jni else '-roots' if args.roots_only else ''
        (BUILD / f'{engine.name}{suffix}.log').write_text(output)
        if result.returncode or ' passed' not in result.stdout or 'terminated with error' in output:
            raise RuntimeError(f'{engine}:\n{output}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)


if __name__ == '__main__':
    main()
