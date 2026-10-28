#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Compare scoped values and Java 25 structured concurrency with the JDK."""
import argparse
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
BUILD = ROOT / '.cache/concurrency-test'
JDK = Path(os.environ['JAVA_HOME'])
PROBES = {
    'ScopedValueProbe': 'Scoped values: nesting, cleanup, nulls, calls, and platform/virtual thread isolation passed',
    'StructuredConcurrencyProbe': 'Structured concurrency: results, cancellation, timeout, ownership, inheritance, and cleanup passed',
}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    parser.add_argument('--reference-only', action='store_true')
    options = parser.parse_args()
    BUILD.mkdir(parents=True, exist_ok=True)
    version_source = BUILD / 'VersionProbe.java'
    version_source.write_text('class VersionProbe { public static void main(String[] a) { System.out.println("version checked"); } }\n')
    subprocess.run([str(JDK / 'bin/javac'), '--enable-preview', '--release', '25', '-d', str(BUILD)]
                   + [str(ROOT / 'test/concurrency' / (name + '.java')) for name in PROBES] + [str(version_source)],
                   check=True, timeout=60)
    engines = [JDK / 'bin/java']
    if not options.reference_only:
        engines.append(options.runtime.resolve())
    for engine in engines:
        for name, marker in PROBES.items():
            flags = ['--enable-preview'] if name == 'StructuredConcurrencyProbe' else []
            result = subprocess.run([str(engine)] + flags + ['-cp', str(BUILD), name],
                                    capture_output=True, text=True, timeout=180)
            (BUILD / f'{engine.name}-{name}.log').write_text(result.stdout + result.stderr)
            if result.returncode or marker not in result.stdout or 'terminated with error' in result.stderr:
                raise AssertionError(f'{engine} {name}: exit={result.returncode}\n{result.stdout}\n{result.stderr}')
            print(f'{engine.name}: {result.stdout.strip()}', flush=True)
        rejected = subprocess.run([str(engine), '-cp', str(BUILD), 'StructuredConcurrencyProbe'],
                                  capture_output=True, text=True, timeout=60)
        if rejected.returncode == 0 or 'UnsupportedClassVersionError' not in rejected.stderr:
            raise AssertionError(f'{engine} accepted preview code without --enable-preview: {rejected.stderr}')
        print(f'{engine.name}: preview class files require --enable-preview', flush=True)
        original = (BUILD / 'VersionProbe.class').read_bytes()
        for major, minor, preview, accepted in [(69, 65535, True, True), (69, 65535, False, False),
                                               (68, 65535, True, False), (69, 1, True, False),
                                               (70, 0, True, False)]:
            version_dir = BUILD / f'version-{major}-{minor}'
            version_dir.mkdir(exist_ok=True)
            patched = bytearray(original)
            patched[4:6] = minor.to_bytes(2, 'big')
            patched[6:8] = major.to_bytes(2, 'big')
            (version_dir / 'VersionProbe.class').write_bytes(patched)
            result = subprocess.run([str(engine)] + (['--enable-preview'] if preview else [])
                                    + ['-cp', str(version_dir), 'VersionProbe'],
                                    capture_output=True, text=True, timeout=60)
            if accepted:
                valid = result.returncode == 0 and result.stdout.strip() == 'version checked'
            else:
                valid = result.returncode != 0 and 'UnsupportedClassVersionError' in result.stderr
            if not valid:
                raise AssertionError(f'{engine} version {major}.{minor}, preview={preview}: {result.stdout}\n{result.stderr}')
        print(f'{engine.name}: Java 25 preview and unsupported class-file version checks passed', flush=True)


if __name__ == '__main__':
    main()
