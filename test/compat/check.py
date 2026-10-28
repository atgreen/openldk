#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Compare module access, Java 25 constructors/imports, strict FP, and charsets."""
import argparse
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
BUILD = ROOT / '.cache/compat-test'
JDK = Path(os.environ['JAVA_HOME'])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--probe', choices=['AccessProbe', 'BytecodeAccessProbe', 'LookupProbe', 'ConstructionProbe', 'FloatingProbe', 'EncodingProbe'])
    parser.add_argument('--reference-only', action='store_true')
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    args = parser.parse_args()
    BUILD.mkdir(parents=True, exist_ok=True)
    sources = ROOT / 'test/compat'
    modules = BUILD / 'modules/access.fixture'
    modules.mkdir(parents=True, exist_ok=True)
    subprocess.run([str(JDK / 'bin/javac'), '-g', '--add-modules', 'java.logging',
                    '--add-reads', 'access.fixture=java.logging', '-d', str(modules)]
                   + [str(p) for p in sorted((sources / 'modules').rglob('*.java'))], check=True, timeout=60)
    probes = [('BytecodeAccessProbe', (25,), [], []), ('LookupProbe', (25,), [], []), ('AccessProbe', (25,), [], [str(modules.parent)]),
              ('ConstructionProbe', (25,), [], []),
              ('ConstructionProbe', (25,), ['--source', '25'], []),
              ('FloatingProbe', (17, 25), [], []),
              ('EncodingProbe', (18, 25), [], ['UTF-8']),
              ('EncodingProbe', (25,), ['-Dfile.encoding=ISO-8859-1'], ['ISO-8859-1'])]
    engines = [JDK / 'bin/java'] + ([] if args.reference_only else [args.runtime.resolve()])
    failures = []
    for probe, releases, flags, arguments in probes:
        if args.probe and args.probe != probe:
            continue
        for release in releases:
            directory = BUILD / f'{probe}-{release}'
            directory.mkdir(parents=True, exist_ok=True)
            compiler_flags = ['--release', str(release)]
            if probe == 'BytecodeAccessProbe':
                compiler_flags = ['--add-exports', 'java.base/jdk.internal.misc=ALL-UNNAMED',
                                  '--add-exports', 'java.base/sun.security.x509=ALL-UNNAMED']
            subprocess.run([str(JDK / 'bin/javac'), '-g'] + compiler_flags + ['-d', str(directory),
                            str(sources / (probe + '.java'))], check=True, timeout=60)
            reference = None
            for engine in engines:
                command = ([str(engine)] + flags + [str(sources / (probe + '.java'))] if '--source' in flags
                           else [str(engine)] + flags + ['-cp', str(directory), probe] + arguments)
                try:
                    result = subprocess.run(command, capture_output=True, text=True, timeout=240)
                    output = result.stdout + result.stderr
                    passed = result.returncode == 0 and ' passed' in result.stdout and 'terminated with error' not in output
                    if reference is None:
                        reference = result.stdout
                    else:
                        passed = passed and reference == result.stdout
                except subprocess.TimeoutExpired as error:
                    output = (error.stdout or b'').decode() + (error.stderr or b'').decode() + '\nTIMEOUT\n'
                    passed = False
                suffix = '-source' if '--source' in flags else ('-override' if flags else '')
                (directory / (engine.name + suffix + '.log')).write_text(output)
                print(f'{engine.name} {probe} --release {release} {flags}: {"PASS" if passed else "FAIL"}', flush=True)
                if not passed:
                    print(output, flush=True)
                    failures.append(command)
    if failures:
        raise SystemExit(f'{len(failures)} comparisons failed')


if __name__ == '__main__':
    main()
