#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Compare Java buffered-image operations with the JDK without native AWT libraries."""
import argparse
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
JDK = Path(os.environ['JAVA_HOME'])
BUILD = ROOT / '.cache/awt-test'
MARKER = 'BufferedImage pixels, shared subimages, and bulk access passed'


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    options = parser.parse_args()
    BUILD.mkdir(parents=True, exist_ok=True)
    subprocess.run([str(JDK / 'bin/javac'), '-d', str(BUILD),
                    *[str(p) for p in (ROOT / 'test/awt').glob('*.java')]], check=True, timeout=60)
    for engine in [JDK / 'bin/java', options.runtime.resolve()]:
        result = subprocess.run([str(engine), '-Djava.awt.headless=true', '-cp',
                                 str(BUILD), 'BufferedImageProbe'], cwd=ROOT,
                                capture_output=True, text=True, timeout=120)
        (BUILD / (engine.name + '.log')).write_text(result.stdout + result.stderr)
        if result.returncode or MARKER not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)
        result = subprocess.run([str(engine), '-Djava.awt.headless=true', '-cp', str(BUILD),
                                 'FontProbe', 'discover'], cwd=ROOT,
                                capture_output=True, text=True, timeout=120)
        (BUILD / (engine.name + '-fonts.log')).write_text(result.stdout + result.stderr)
        if result.returncode or 'Font family discovery passed' not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)
        pixels = BUILD / (engine.name + '.pixels')
        result = subprocess.run([str(engine), '-Djava.awt.headless=true', '-cp', str(BUILD),
                                 'GraphicsProbe', str(pixels)], cwd=ROOT,
                                capture_output=True, text=True, timeout=120)
        (BUILD / (engine.name + '-graphics.log')).write_text(result.stdout + result.stderr)
        if result.returncode or 'Graphics2D image transfer passed' not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)
    reference = (BUILD / 'java.pixels').read_bytes()
    actual = (BUILD / (options.runtime.name + '.pixels')).read_bytes()
    if len(reference) != 80 * 64 * 4 or len(actual) != len(reference):
        raise AssertionError('Incomplete rendered pixel data')
    # Native loops quantize premultiplied channels. Allow two 8-bit levels
    # for that rounding, including pixels with very small alpha values.
    for index in range(0, len(reference), 4):
        ra, *rc = reference[index:index + 4]
        aa, *ac = actual[index:index + 4]
        if abs(ra - aa) > 1 or any(abs(r * ra - a * aa) > 2 * 255 for r, a in zip(rc, ac)):
            x, y = (index // 4) % 80, (index // 4) // 80
            raise AssertionError(f'Rendered pixel differs at {x}, {y}: '
                                 f'{reference[index:index + 4].hex()} vs {actual[index:index + 4].hex()}')
    print('Graphics2D rendered pixels match JDK within alpha-compositing rounding', flush=True)


if __name__ == '__main__':
    main()
