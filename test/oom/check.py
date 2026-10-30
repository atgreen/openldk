#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Exercise array exhaustion and recovery in disposable, small-heap processes."""
import argparse
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
JDK = Path(os.environ['JAVA_HOME'])
BUILD = ROOT / '.cache/oom-test'
BUILD.mkdir(parents=True, exist_ok=True)
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--runtime', type=Path, required=True,
                    help='OpenLDK executable built with a 512 MiB dynamic space')
parser.add_argument('--native-recovery', action='store_true', help='also exercise genuine SBCL exhaustion')
args = parser.parse_args()
subprocess.run([str(JDK / 'bin/javac'), '--release', '25', '-d', str(BUILD),
                str(ROOT / 'test/oom/ArrayOOMProbe.java'),
                str(ROOT / 'testsuite/gcj/newarray_overflow.java')], check=True, timeout=60)
for case in ('oversized', 'primitive', 'reference', 'reflect-primitive', 'reflect-reference',
             'multi', 'reflect-multi', 'concurrent', 'newarray_overflow'):
    for engine in (JDK / 'bin/java', args.runtime.resolve()):
        command = [str(engine)] + (['-Xmx128m'] if engine == JDK / 'bin/java' else [])
        command += ['-cp', str(BUILD)]
        command += ['newarray_overflow'] if case == 'newarray_overflow' else ['ArrayOOMProbe', case]
        result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, timeout=180)
        (BUILD / f'{engine.name}-{case}.log').write_text(result.stdout + result.stderr)
        expected = 'ok' if case == 'newarray_overflow' else f'{case}: caught OutOfMemoryError and recovered'
        if result.returncode or result.stdout.strip() != expected or result.stderr:
            raise RuntimeError(f'{engine.name} {case}:\n{result.stdout}{result.stderr}')
        print(f'PASS {engine.name} {case}', flush=True)
if args.native_recovery:
    environment = os.environ.copy()
    environment['CL_SOURCE_REGISTRY'] = str(ROOT) + '//:'
    environment['XDG_CACHE_HOME'] = str(ROOT / '.cache')
    result = subprocess.run(['sbcl', '--dynamic-space-size', '512', '--noinform', '--disable-debugger',
                             '--load', str(ROOT / 'test/oom/native-recovery.lisp'), '--quit'],
                            cwd=ROOT, env=environment, capture_output=True, text=True, timeout=240)
    (BUILD / 'native-recovery.log').write_text(result.stdout + result.stderr)
    if result.returncode or 'Native exhaustion: Java OutOfMemoryError caught' not in result.stdout:
        raise RuntimeError('Native recovery failed; see .cache/oom-test/native-recovery.log')
    print('PASS native recovery: three real SBCL exhaustion cycles', flush=True)
