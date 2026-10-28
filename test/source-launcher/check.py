#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Compare Java source-file launches with JDK 25; applications run in OpenLDK."""
import argparse
import os
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
BUILD = ROOT / '.cache/source-launcher-test'
JDK = Path(os.environ['JAVA_HOME'])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    parser.add_argument('--reference-only', action='store_true')
    options = parser.parse_args()
    BUILD.mkdir(parents=True, exist_ok=True)
    engines = [JDK / 'bin/java']
    if not options.reference_only:
        engines.append(options.runtime.resolve())
    with tempfile.TemporaryDirectory(prefix='sources-', dir=BUILD) as temporary:
        directory = Path(temporary)
        def source(name, text):
            path = directory / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(text + '\n')
            return str(path)
        cases = []
        def case(name, text, expected=None, *, flags=(), code=0, error=None, arguments=()):
            cases.append((name, list(flags) + [source(name, text)] + list(arguments), expected, code, error))
        case('Classic.java', 'class Classic { public static void main(String[] a) { System.out.println(String.join("|", a)); } }',
             'one||λ|-x', arguments=['one', '', 'λ', '-x'])
        case('Compact.java', 'void main() { System.out.println("compact"); }', 'compact')
        case('Imports.java', 'void main() { List<String> words = List.of("implicit", "imports"); IO.println(String.join(" ", words)); }', 'implicit imports')
        case('Odd Name.java', 'public class DifferentName { void main() { System.out.println("different-name"); } }', 'different-name')
        case('Order.java', 'class First { void main() { System.out.println("first"); } } class Second { void main() { System.out.println("second"); } }', 'first')
        case('p/Packaged.java', 'package p; class Packaged { void main() { System.out.println(Helper.message()); } }', 'source-dependency')
        source('p/Helper.java', 'package p; class Helper { static String message() { return "source-dependency"; } }')
        case('Property.java', 'void main() { System.out.println(System.getProperty("source.test")); }', 'property-value', flags=['-Dsource.test=property-value'])
        case('Runtime.java', 'void main() { System.out.println(System.getProperty("java.vm.name")); }', '@runtime')
        case('Location.java', 'void main() { System.out.println(System.getProperty("java.class.path")); }', '@classpath')
        case('script', '#!/usr/bin/env -S openldk --source 25\nvoid main(String[] a) { System.out.println("script:" + a[0]); }',
             'script:λ', flags=['--source', '25'], arguments=['λ'])
        case('Old.java', 'class Old { public static void main(String[] a) { System.out.println("release-17"); } }', 'release-17', flags=['--source', '17'])
        case('EqualsOption.java', 'void main() { System.out.println("equals-option"); }', 'equals-option', flags=['--source=25'])
        case('Preview.java', 'void main() throws Exception { try (var scope = java.util.concurrent.StructuredTaskScope.<Integer>open()) { var n = scope.fork(() -> 42); scope.join(); System.out.println(n.get()); } }',
             '42', flags=['--enable-preview'])
        case('PreviewOff.java', 'void main() { java.util.concurrent.StructuredTaskScope.open(); }', code=1, error='preview')
        case('Broken.java', 'void main() { not valid java }', code=1, error='error')
        case('MissingMain.java', 'class MissingMain {}', code=1)
        case('FirstHasNoMain.java', 'class FirstHasNoMain {} class HasMain { void main() { System.out.println("wrong-main"); } }', code=1)
        case('Throws.java', 'void main() { throw new IllegalStateException("source-boom"); }', code=1, error='source-boom')
        case('Exits.java', 'void main() { System.exit(7); }', code=7)
        case('ExitsThread.java', 'void main() throws Exception { Thread.ofPlatform().start(() -> System.exit(9)); Thread.sleep(60_000); }', code=9)
        cases.append(('Missing.java', [str(directory / 'Missing.java')], None, 1, None))
        source('external/External.java', 'package external; public class External { public static String message() { return "classpath"; } }')
        classes = directory / 'classes'
        subprocess.run([str(JDK / 'bin/javac'), '-d', str(classes), str(directory / 'external/External.java')], check=True, timeout=60)
        case('Classpath.java', 'void main() { System.out.println(external.External.message()); }', 'classpath', flags=['-cp', str(classes)])
        before_classes = set(directory.rglob('*.class'))
        for engine in engines:
            reference = engine == JDK / 'bin/java'
            for name, launch, expected, code, error in cases:
                scratch = set(path for path in Path(tempfile.gettempdir()).glob('openldk-source-*') if path.is_dir())
                result = subprocess.run([str(engine)] + launch, cwd=directory, capture_output=True, text=True, timeout=180)
                (BUILD / f'{engine.name}-{Path(name).name}.log').write_text(result.stdout + result.stderr)
                actual = result.stdout.strip()
                if expected == '@runtime':
                    valid = ('OpenJDK' in actual) if reference else ('OpenLDK' in actual)
                elif expected == '@classpath':
                    valid = True
                    if not reference:
                        compiled = Path(actual.split(os.pathsep)[0])
                        valid = compiled.name == 'classes' and not compiled.parent.exists()
                else:
                    valid = actual == (expected or '')
                valid &= result.returncode == code
                if error:
                    valid &= error.lower() in result.stderr.lower()
                if not valid:
                    raise AssertionError(f'{engine} {name}: exit={result.returncode}\n{result.stdout}\n{result.stderr}')
                if set(directory.rglob('*.class')) != before_classes:
                    raise AssertionError(f'{engine} left class files beside source input')
                remaining = set(path for path in Path(tempfile.gettempdir()).glob('openldk-source-*') if path.is_dir())
                if remaining - scratch:
                    raise AssertionError(f'{engine} left temporary compiler output: {remaining - scratch}')
            print(f'{engine.name}: {len(cases)} source launches, compilation failures, arguments, dependencies, preview, and runtime checks passed', flush=True)


if __name__ == '__main__':
    main()
