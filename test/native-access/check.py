#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Compare native-access policy, permissions and launcher forms with JDK 25."""
import argparse
import os
from pathlib import Path
import re
import shlex
import subprocess
import zipfile

ROOT = Path(__file__).resolve().parents[2]
JDK = Path(os.environ['JAVA_HOME'])
BUILD = ROOT / '.cache/native-access-test'
SOURCE = ROOT / 'test/native-access'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    parser.add_argument('--reference-only', action='store_true')
    parser.add_argument('--match', help='run only case names containing this text')
    args = parser.parse_args()
    BUILD.mkdir(parents=True, exist_ok=True)
    subprocess.run([str(JDK / 'bin/javac'), '--release', '25', '-d', str(BUILD),
                    str(SOURCE / 'NativeAccessProbe.java'), str(SOURCE / 'NamedNativeAccessProbe.java')], check=True, timeout=60)
    library = BUILD / 'libpolicy.so'
    subprocess.run(shlex.split(os.environ.get('CC', 'cc')) + ['-Wall', '-Wextra', '-Werror', '-shared', '-fPIC',
                    f'-I{JDK}/include', f'-I{JDK}/include/linux', str(SOURCE / 'probe.c'), '-o', str(library)], check=True, timeout=60)
    modules = BUILD / 'modules'
    subprocess.run([str(JDK / 'bin/javac'), '--release', '25', '--module-source-path', str(SOURCE / 'modules'),
                    '-d', str(modules)] + [str(path) for path in sorted((SOURCE / 'modules').rglob('*.java'))], check=True, timeout=60)
    named_library = BUILD / 'libnamedpolicy.so'
    subprocess.run(shlex.split(os.environ.get('CC', 'cc')) + ['-Wall', '-Wextra', '-Werror', '-shared', '-fPIC',
                    f'-I{JDK}/include', f'-I{JDK}/include/linux', str(SOURCE / 'named.c'), '-o', str(named_library)], check=True, timeout=60)
    engines = [JDK / 'bin/java'] + ([] if args.reference_only else [args.runtime.resolve()])
    env = os.environ.copy()
    env.pop('JDK_JAVA_OPTIONS', None)
    options = {
        'default': [], 'warn': ['--illegal-native-access=warn'],
        'allow': ['--illegal-native-access=allow'], 'deny': ['--illegal-native-access=deny'],
        'enabled': ['--enable-native-access=ALL-UNNAMED', '--illegal-native-access=deny'],
        'separated': ['--enable-native-access', 'ALL-UNNAMED', '--illegal-native-access=deny'],
        'repeated': ['--enable-native-access=java.base', '--enable-native-access=ALL-UNNAMED', '--illegal-native-access=deny'],
        'unknown': ['--enable-native-access=openldk.missing'],
    }
    cases = []
    for name, flags in options.items():
        for mode in ['jni', 'ffm', 'binding', 'mixed', 'null', 'relative', 'missing', 'path', 'runtime']:
            cases.append((f'{name}-{mode}', flags + ['-cp', str(BUILD), 'NativeAccessProbe', mode, str(library)], env, True))
    for name, flags in [('default', []), ('enabled', ['--enable-native-access=java.logging']),
                        ('repeated', ['--enable-native-access=java.logging', '--enable-native-access=ALL-UNNAMED'])]:
        cases.append((f'boot-{name}', flags + ['--illegal-native-access=deny', '-cp', str(BUILD), 'NativeAccessProbe', 'boot'], env, True))
    for policy in ['allow', 'warn', 'deny']:
        for grant in [False, True]:
            arguments = ['--enable-native-access=ALL-UNNAMED', f'--illegal-native-access={policy}',
                         '-cp', str(BUILD), 'NamedNativeAccessProbe', str(modules), str(named_library)]
            if grant:
                arguments.append('grant')
            cases.append((f'named-{policy}-{grant}', arguments, env, True))
    argfile = BUILD / 'native.args'
    argfile.write_text('# Native access permissions\n--enable-native-access=ALL-UNNAMED\n--illegal-native-access=deny\n')
    cases.append(('argfile', ['@' + str(argfile), '-cp', str(BUILD), 'NativeAccessProbe', 'mixed', str(library)], env, True))
    native_env = dict(env, JDK_JAVA_OPTIONS='--enable-native-access=ALL-UNNAMED --illegal-native-access=deny')
    cases.append(('environment', ['-cp', str(BUILD), 'NativeAccessProbe', 'mixed', str(library)], native_env, True))
    bad_environment = dict(env, JDK_JAVA_OPTIONS=f'-cp {BUILD} NativeAccessProbe binding')
    cases.append(('environment-main-rejected', ['-cp', str(BUILD), 'NativeAccessProbe', 'ffm'], bad_environment, False))
    quoted_env = dict(env, JDK_JAVA_OPTIONS='--enable-native-access "ALL-UNNAMED" --illegal-native-access="deny"')
    cases.append(('environment-quoted', ['-cp', str(BUILD), 'NativeAccessProbe', 'mixed', str(library)], quoted_env, True))
    bad_quote = dict(env, JDK_JAVA_OPTIONS='--enable-native-access="ALL-UNNAMED')
    cases.append(('environment-bad-quote', ['-cp', str(BUILD), 'NativeAccessProbe', 'ffm'], bad_quote, False))
    cases.append(('argument-expansion-disabled', ['@' + str(argfile), '--disable-@files', '-cp', str(BUILD),
                  'NativeAccessProbe', 'ffm', '@unexpanded-file'], env, True))
    cases.append(('source-enabled', ['--enable-native-access=ALL-UNNAMED', '--illegal-native-access=deny',
                  str(SOURCE / 'NativeAccessProbe.java'), 'mixed', str(library)], env, True))
    for name, value, success in [('enabled', 'ALL-UNNAMED', True), ('invalid', 'java.base', False)]:
        jar = BUILD / f'{name}.jar'
        with zipfile.ZipFile(jar, 'w') as archive:
            archive.writestr('META-INF/MANIFEST.MF', 'Manifest-Version: 1.0\nMain-Class: NativeAccessProbe\n'
                             f'Enable-Native-Access: {value}\n\n')
            archive.write(BUILD / 'NativeAccessProbe.class', 'NativeAccessProbe.class')
        cases.append((f'manifest-{name}', ['--illegal-native-access=deny', '-jar', str(jar), 'mixed', str(library)], env, success))
    for name, flags in [('missing-enable', ['--enable-native-access']),
                        ('invalid-policy', ['--illegal-native-access=invalid']),
                        ('empty-enable', ['--enable-native-access='])]:
        arguments = flags if name == 'missing-enable' else flags + ['-cp', str(BUILD), 'NativeAccessProbe', 'ffm']
        cases.append((name, arguments, env, False))
    failures = []
    for name, arguments, environment, success in cases:
        if args.match and args.match not in name:
            continue
        reference = None
        for engine in engines:
            result = subprocess.run([str(engine)] + arguments, env=environment, capture_output=True, text=True, timeout=180)
            output = result.stdout + result.stderr
            (BUILD / f'{engine.name}-{name}.log').write_text(output)
            warnings = tuple(len(re.findall(pattern, result.stderr)) for pattern in
                             [r'WARNING: A restricted method', r'WARNING: A native method', r'WARNING: Unknown module'])
            passed = (result.returncode == 0) == success
            if success:
                passed = passed and 'Native access policy passed' in result.stdout and 'terminated with error' not in output
                value = (result.stdout, warnings)
                if reference is None:
                    reference = value
                else:
                    passed = passed and value == reference
            print(f'{engine.name} native-access {name}: {"PASS" if passed else "FAIL"}', flush=True)
            if not passed:
                print(output, flush=True)
                failures.append(f'{engine.name} {name}')
    if failures:
        raise SystemExit('Failed comparisons: ' + ', '.join(failures))


if __name__ == '__main__':
    main()
