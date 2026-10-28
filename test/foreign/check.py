#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Exercise FFM through the system linker and ordinary C libraries."""
import argparse
import os
from pathlib import Path
import shlex
import subprocess

ROOT = Path(__file__).resolve().parents[2]
BUILD = ROOT / '.cache/foreign-test'
JDK = Path(os.environ['JAVA_HOME'])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    parser.add_argument('--reference-only', action='store_true')
    options = parser.parse_args()
    BUILD.mkdir(parents=True, exist_ok=True)
    library = BUILD / 'libprobe.so'
    subprocess.run(shlex.split(os.environ.get('CC', 'cc')) + ['-std=c11', '-shared', '-fPIC',
                    '-Wall', '-Wextra', '-Werror', str(ROOT / 'test/foreign/probe.c'), '-pthread',
                    '-o', str(library)], check=True, timeout=60)
    subprocess.run([str(JDK / 'bin/javac'), '-d', str(BUILD),
                    str(ROOT / 'test/foreign/ForeignProbe.java')], check=True, timeout=60)
    engines = [(JDK / 'bin/java', ['--enable-native-access=ALL-UNNAMED'])]
    if not options.reference_only:
        engines.append((options.runtime.resolve(), []))
    marker = 'FFM strlen, scalar ABI, pointers, structs, variadics, errno, heap aliases, lifetimes, qsort, and native-thread upcalls passed'
    for engine, flags in engines:
        result = subprocess.run([str(engine)] + flags + ['-cp', str(BUILD), 'ForeignProbe', str(library)],
                                capture_output=True, text=True, timeout=180)
        (BUILD / (engine.name + '.log')).write_text(result.stdout + result.stderr)
        if result.returncode or marker not in result.stdout or 'terminated with error' in result.stderr:
            raise RuntimeError(f'{engine}: exit {result.returncode}\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)


if __name__ == '__main__':
    main()
