#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""A loader calls a method again while its first invocation is linking a class."""
import argparse
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
JDK = Path(os.environ['JAVA_HOME'])
BUILD = ROOT / '.cache/reentrant-test'


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    options = parser.parse_args()
    BUILD.mkdir(parents=True, exist_ok=True)
    subprocess.run([str(JDK / 'bin/javac'), '-d', str(BUILD)]
                   + [str(p) for p in (ROOT / 'test/reentrant').glob('*.java')], check=True, timeout=60)
    for engine in [JDK / 'bin/java', options.runtime.resolve()]:
        result = subprocess.run([str(engine), '-cp', str(BUILD), 'ReentrantCompilationProbe', str(BUILD)],
                                cwd=ROOT, capture_output=True, text=True, timeout=120)
        (BUILD / (engine.name + '.log')).write_text(result.stdout + result.stderr)
        if result.returncode or 'Reentrant class loading, arithmetic, exception handlers, and monitors passed' not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)
        for probe, marker in [('StaticMethodProbe', 'Inherited static method handles initialize only their declaring class'),
                              ('InterfaceInitProbe', 'Inherited interface constant initialized before use'),
                              ('PrimitiveMapperProbe', 'Primitive SAM return unboxing, widening, null, and type checks passed')]:
            result = subprocess.run([str(engine), '-cp', str(BUILD), probe],
                                    cwd=ROOT, capture_output=True, text=True, timeout=120)
            if result.returncode or marker not in result.stdout:
                raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
            print(f'{engine.name}: {result.stdout.strip()}', flush=True)
        result = subprocess.run([str(engine), '-cp', str(BUILD), 'StringInternProbe'],
                                cwd=ROOT, capture_output=True, text=True, timeout=120)
        if result.returncode or 'Concurrent string interning preserves canonical identity' not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)
        result = subprocess.run([str(engine), '-cp', str(BUILD), 'ReflectionLoaderProbe', str(BUILD)],
                                cwd=ROOT, capture_output=True, text=True, timeout=120)
        if result.returncode or 'static fields preserve defining loaders' not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)
        result = subprocess.run([str(engine), '-cp', str(BUILD), 'ReflectionProbe'],
                                cwd=ROOT, capture_output=True, text=True, timeout=120)
        if result.returncode or 'Reflection target wrapping' not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)


if __name__ == '__main__':
    main()
