#!/usr/bin/env python3
# Copyright (C) 2026 Anthony Green <green@moxielogic.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Compare runtime standards behavior with the installed JDK 25 reference runtime."""
import argparse
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
BUILD = ROOT / '.cache/standards-test'
JDK = Path(os.environ['JAVA_HOME'])
PROBES = {
    'LoaderConstraintProbe': (25,),
    'ClassLoaderPackageProbe': (25,),
    'ArrayReflectionProbe': (25,),
    'ArrayExceptionProbe': (25,),
    'ReflectionConversionProbe': (25,),
    'ReflectionDispatchProbe': (25,),
    'FloatingArithmeticProbe': (25,),
    'FieldTypeProbe': (25,),
    'PrimitiveOptimizationProbe': (25,),
    'PrimitiveResultProbe': (25,),
    'MethodHandleGraphProbe': (25,),
    'InlineLeafProbe': (25,),
    'CallCheckProbe': (25,),
    'InstanceInlineProbe': (25,),
    'ArrayLoopProbe': (25,),
    'LoaderLifetimeProbe': (25,),
    'LinkageProbe': (25,),
    'MemoryModelProbe': (25,),
    'BufferViewProbe': (25,),
    'ConcurrencyStressProbe': (25,),
    'CpuTimeProbe': (25,),
    'ThreadFailureProbe': (25,),
    'MainThreadProbe': (25,),
    'MainJoinProbe': (25,),
    'DatagramProbe': (25,),
    'MismatchProbe': (25,),
    'DataCharacterProbe': (25,),
    'RandomAccessLengthProbe': (25,),
    'ZipFailureProbe': (25,),
    'FileDescriptorCloseProbe': (25,),
    'MappedMemoryProbe': (25,),
    'CryptoProbe': (25,),
    'VectorProbe': (25,),
    'ReferenceProbe': (25,),
}


def check_module_launches(engines):
    """Exercise root-module parsing, image graph rebuilding, and source compilation."""
    directory = BUILD / 'VectorProbe-25'
    cases = {
        'equals': (['--add-modules=jdk.incubator.vector', '-cp', str(directory), 'VectorProbe'], True),
        'source-repeated': (['--add-modules', 'java.xml', '--add-modules', 'jdk.incubator.vector',
                             str(ROOT / 'test/standards/VectorProbe.java')], True),
        'missing-argument': (['--add-modules'], False),
        'unknown-module': (['--add-modules', 'openldk.missing', '-cp', str(directory), 'VectorProbe'], False),
    }
    failures = []
    for name, (arguments, success) in cases.items():
        reference = None
        for engine in engines:
            try:
                result = subprocess.run([str(engine)] + arguments, capture_output=True, text=True, timeout=240)
                output = result.stdout + result.stderr
                passed = (result.returncode == 0) == success
                if success:
                    passed = passed and ' passed' in result.stdout and 'terminated with error' not in output
                    if reference is None:
                        reference = result.stdout
                    else:
                        passed = passed and result.stdout == reference
            except subprocess.TimeoutExpired:
                output = 'TIMEOUT'
                passed = False
            (directory / f'{engine.name}-{name}.log').write_text(output)
            print(f'{engine.name} module launch {name}: {"PASS" if passed else "FAIL"}', flush=True)
            if not passed:
                print(output, flush=True)
                failures.append(f'{engine.name} module launch {name}')
    return failures


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
            flags = ['--enable-preview'] if probe == 'CryptoProbe' else []
            if probe == 'VectorProbe':
                flags += ['--add-modules', 'jdk.incubator.vector']
            subprocess.run([str(JDK / 'bin/javac'), '--release', str(release)] + flags
                           + ['-g', '-d', str(directory), str(ROOT / 'test/standards' / (probe + '.java'))]
                           + ([str(p) for p in sorted((ROOT / 'test/standards/dispatch').rglob('*.java'))]
                              if probe == 'ReflectionDispatchProbe' else [])
                           + ([str(p) for p in sorted((ROOT / 'test/standards/packages').rglob('*.java'))]
                              if probe == 'ClassLoaderPackageProbe' else []),
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
    if args.probe in (None, 'VectorProbe'):
        failures.extend(check_module_launches(engines))
    if failures:
        raise SystemExit('Failed comparisons: ' + ', '.join(failures))


if __name__ == '__main__':
    main()
