#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Compare modern library behavior with the installed JDK 25 reference runtime."""
import argparse
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
BUILD = ROOT / '.cache/modern-test'
JDK = Path(os.environ['JAVA_HOME'])
PROBES = {
    'ClassFileProbe': (24, 25),
    'GathererProbe': (24, 25),
    'SequencedProbe': (21, 25),
    'StableValueProbe': (25,),
}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    parser.add_argument('--reference-only', action='store_true')
    parser.add_argument('--probe', choices=PROBES)
    args = parser.parse_args()
    engines = [JDK / 'bin/java']
    if not args.reference_only:
        engines.append(args.runtime.resolve())
    failures = []
    for probe, releases in PROBES.items():
        if args.probe and args.probe != probe:
            continue
        for release in releases:
            directory = BUILD / f'{probe}-{release}'
            directory.mkdir(parents=True, exist_ok=True)
            flags = ['--enable-preview'] if probe == 'StableValueProbe' else []
            subprocess.run([str(JDK / 'bin/javac'), '--release', str(release)] + flags
                           + ['-g', '-d', str(directory), str(ROOT / 'test/modern' / (probe + '.java'))],
                           check=True, timeout=60)
            reference_output = None
            for engine in engines:
                command = [str(engine)] + flags + ['-cp', str(directory), probe]
                try:
                    result = subprocess.run(command, capture_output=True, text=True, timeout=240)
                    output = result.stdout + result.stderr
                    passed = result.returncode == 0 and ' passed' in result.stdout and 'terminated with error' not in output
                    if reference_output is None:
                        reference_output = result.stdout
                    else:
                        passed = passed and result.stdout == reference_output
                except subprocess.TimeoutExpired as error:
                    output = (error.stdout or b'').decode() + (error.stderr or b'').decode() + '\nTIMEOUT\n'
                    passed = False
                (directory / f'{engine.name}.log').write_text(output)
                print(f'{engine.name} --release {release} {probe}: {"PASS" if passed else "FAIL"}', flush=True)
                if not passed:
                    print(output, flush=True)
                    failures.append(f'{engine.name} --release {release} {probe}')
    if failures:
        raise SystemExit('Failed comparisons: ' + ', '.join(failures))


if __name__ == '__main__':
    main()
