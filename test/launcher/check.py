#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Compare Java 25 entry-point selection with the reference launcher."""
import argparse
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
BUILD = ROOT / '.cache/launcher-test'
JDK = Path(os.environ['JAVA_HOME'])

# Each source is compiled by javac 25; compact source files need no preview flag.
SOURCES = {
    'Classic': 'public class Classic { public static void main(String[] a) { System.out.println(String.join("|", a)); } }',
    'StaticZero': 'class StaticZero { static void main() { System.out.println("static-zero"); } }',
    'InstanceZero': 'class InstanceZero { int value; InstanceZero() { value = 42; } void main() { System.out.println("instance-" + value); } }',
    'InstanceArgs': 'class InstanceArgs { protected void main(String... a) { System.out.println("instance:" + String.join("|", a)); } }',
    'Compact': 'void main() { System.out.println("compact"); }',
    'CompactArgs': 'void main(String[] a) { System.out.println("compact:" + String.join("|", a)); }',
    'ArrayWins': 'class ArrayWins { static void main() { throw new AssertionError("wrong-main"); } void main(String[] a) { System.out.println("array-wins"); } }',
    'StaticWins': 'class StaticWins { private StaticWins() { throw new AssertionError("constructed"); } void main() { throw new AssertionError("wrong-main"); } static void main(String[] a) { System.out.println("static-wins"); } }',
    'ProtectedStatic': 'class ProtectedStatic { protected static void main() { System.out.println("protected-static"); } }',
    'InheritedStatic': 'class StaticParent { public static void main(String[] a) { System.out.println("inherited-static"); } } class InheritedStatic extends StaticParent { static void main() { throw new AssertionError("wrong-main"); } }',
    'InheritedInstance': 'class InstanceParent { protected void main() { System.out.println(getClass().getSimpleName()); } } class InheritedInstance extends InstanceParent { InheritedInstance() { System.out.print("child-ctor:"); } }',
    'DefaultMain': 'interface MainDefault { default void main(String[] a) { System.out.println("default:" + getClass().getSimpleName()); } } class DefaultMain implements MainDefault {}',
    'SpecificDefault': 'interface RootMain { default void main() { System.out.println("root"); } } interface SubMain extends RootMain { default void main() { System.out.println("sub"); } } class SpecificDefault implements RootMain, SubMain {}',
    'InterfaceStatic': 'interface InterfaceStatic { static void main() { System.out.println("interface-static"); } }',
    'StaticNotInherited': 'class StaticNotInherited implements InterfaceStatic {}',
    'PrivateFallback': 'class PrivateFallback { private static void main(String[] a) { throw new AssertionError("wrong-main"); } static void main() { System.out.println("private-fallback"); } }',
    'PrivateOnly': 'class PrivateOnly { private static void main(String[] a) {} }',
    'WrongReturn': 'class WrongReturn { static int main(String[] a) { return 0; } }',
    'WrongParameter': 'class WrongParameter { static void main(Object[] a) {} }',
    'AbstractInstance': 'abstract class AbstractInstance { void main() {} }',
    'AbstractStatic': 'abstract class AbstractStatic { static void main() { System.out.println("abstract-static"); } }',
    'PrivateCtor': 'class PrivateCtor { private PrivateCtor() {} void main() {} }',
    'MissingCtor': 'class MissingCtor { MissingCtor(int x) {} void main() {} }',
    'NoCtorFallback': 'class NoCtorFallback { private NoCtorFallback() {} static void main() { System.out.println("wrong-main"); } void main(String[] a) {} }',
    'ThrowCtor': 'class ThrowCtor { ThrowCtor() { throw new IllegalStateException("ctor-boom"); } void main() { System.out.println("wrong-main"); } }',
    'ThrowMain': 'class ThrowMain { void main() { throw new IllegalStateException("main-boom"); } }',
    'InitOrder': 'class InitParent { static { System.out.print("parent|"); } InitParent() { System.out.print("super-ctor|"); } } class InitOrder extends InitParent { static { System.out.print("child|"); } InitOrder() { System.out.print("ctor|"); } void main() { System.out.println("main"); } }',
    'EmptyRecord': 'record EmptyRecord() { void main() { System.out.println("record-main"); } }',
    'p/PackageParent': 'package p; public class PackageParent { void main() { System.out.println("package-parent"); } }',
    'q/PackageChild': 'package q; public class PackageChild extends p.PackageParent {}',
}
CASES = [
    ('Classic', 'one||λ', None), ('StaticZero', 'static-zero', None),
    ('InstanceZero', 'instance-42', None), ('InstanceArgs', 'instance:one||λ', None),
    ('Compact', 'compact', None), ('CompactArgs', 'compact:one||λ', None),
    ('ArrayWins', 'array-wins', None), ('StaticWins', 'static-wins', None),
    ('ProtectedStatic', 'protected-static', None), ('InheritedStatic', 'inherited-static', None),
    ('InheritedInstance', 'child-ctor:InheritedInstance', None), ('DefaultMain', 'default:DefaultMain', None),
    ('SpecificDefault', 'sub', None), ('InterfaceStatic', 'interface-static', None),
    ('PrivateFallback', 'private-fallback', None), ('AbstractStatic', 'abstract-static', None),
    ('InitOrder', 'parent|child|super-ctor|ctor|main', None), ('EmptyRecord', 'record-main', None),
    ('StaticNotInherited', None, None), ('PrivateOnly', None, None), ('WrongReturn', None, None),
    ('WrongParameter', None, None), ('AbstractInstance', None, None), ('PrivateCtor', None, None),
    ('MissingCtor', None, None), ('NoCtorFallback', None, None),
    ('ThrowCtor', None, 'ctor-boom'), ('ThrowMain', None, 'main-boom'),
    ('q.PackageChild', 'package-parent', None),
]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    parser.add_argument('--reference-only', action='store_true')
    options = parser.parse_args()
    sources = BUILD / 'sources'
    classes = BUILD / 'classes'
    classes.mkdir(parents=True, exist_ok=True)
    paths = []
    for name, text in SOURCES.items():
        path = sources / (name + '.java')
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text + '\n')
        paths.append(str(path))
    subprocess.run([str(JDK / 'bin/javac'), '--release', '25', '-d', str(classes)] + paths,
                   check=True, timeout=60)
    jar = BUILD / 'instance-main.jar'
    subprocess.run([str(JDK / 'bin/jar'), '--create', '--file', str(jar), '--main-class',
                    'InstanceArgs', '-C', str(classes), '.'], check=True, timeout=60)
    engines = [JDK / 'bin/java']
    if not options.reference_only:
        engines.append(options.runtime.resolve())
    for engine in engines:
        for name, expected, error in CASES + [('jar', 'instance:one||λ', None)]:
            launch = ['-jar', str(jar)] if name == 'jar' else ['-cp', str(classes), name]
            result = subprocess.run([str(engine)] + launch + ['one', '', 'λ'],
                                    cwd=ROOT, capture_output=True, text=True, timeout=120)
            (BUILD / (engine.name + '-' + name + '.log')).write_text(result.stdout + result.stderr)
            if expected is not None:
                valid = result.returncode == 0 and result.stdout.strip() == expected
            else:
                valid = result.returncode != 0 and not result.stdout.strip()
                if error:
                    valid = valid and error in result.stderr
            if not valid:
                raise AssertionError(f'{engine} {name}: exit={result.returncode}\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {len(CASES) + 1} Java 25 main selection, construction, inheritance, rejection, and JAR checks passed', flush=True)


if __name__ == '__main__':
    main()
