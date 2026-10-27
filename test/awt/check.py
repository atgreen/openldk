#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Compare Java buffered-image operations with the JDK without native AWT libraries."""
import argparse
import math
import os
from pathlib import Path
import subprocess
import struct

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
    font_metrics = []
    for engine in [JDK / 'bin/java', options.runtime.resolve()]:
        result = subprocess.run([str(engine), '-Djava.awt.headless=true', '-cp',
                                 str(BUILD), 'BufferedImageProbe'], cwd=ROOT,
                                capture_output=True, text=True, timeout=120)
        (BUILD / (engine.name + '.log')).write_text(result.stdout + result.stderr)
        if result.returncode or MARKER not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)
        result = subprocess.run([str(engine), '-Djava.awt.headless=true', '-cp', str(BUILD),
                                 'ImageProducerProbe'], cwd=ROOT,
                                capture_output=True, text=True, timeout=120)
        (BUILD / (engine.name + '-producer.log')).write_text(result.stdout + result.stderr)
        if result.returncode or 'Filtered image production, pixel grabbing, and toolkit representation passed' not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)
        result = subprocess.run([str(engine), '-Djava.awt.headless=true', '-cp', str(BUILD),
                                 'ConvolveProbe', str(BUILD / (engine.name + '.convolve'))], cwd=ROOT,
                                capture_output=True, text=True, timeout=120)
        (BUILD / (engine.name + '-convolve.log')).write_text(result.stdout + result.stderr)
        if result.returncode or 'Convolution kernels, edges, alpha, grayscale, and rasters passed' not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)
        result = subprocess.run([str(engine), '-Djava.awt.headless=true', '-cp', str(BUILD),
                                 'FontProbe'], cwd=ROOT,
                                capture_output=True, text=True, timeout=120)
        (BUILD / (engine.name + '-fonts.log')).write_text(result.stdout + result.stderr)
        if result.returncode or 'Font glyph advances and outlines passed' not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)
        metric_line = next(line for line in result.stdout.splitlines() if line.startswith('FONT_METRICS '))
        metrics = [float(value) for value in metric_line.split()[1:]]
        if len(metrics) != 5 or not all(math.isfinite(value) for value in metrics):
            raise AssertionError(f'Invalid font metrics: {metrics}')
        font_metrics.append(metrics)
        pixels = BUILD / (engine.name + '.pixels')
        result = subprocess.run([str(engine), '-Djava.awt.headless=true', '-cp', str(BUILD),
                                 'GraphicsProbe', str(pixels)], cwd=ROOT,
                                capture_output=True, text=True, timeout=120)
        (BUILD / (engine.name + '-graphics.log')).write_text(result.stdout + result.stderr)
        if result.returncode or 'Graphics2D image transfer passed' not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)
    if any(abs(a - b) > .5 for a, b in zip(*font_metrics)):
        raise AssertionError(f'Font advance or outline bounds differ: {font_metrics}')
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
    reference = (BUILD / 'java.convolve').read_bytes()
    actual = (BUILD / (options.runtime.name + '.convolve')).read_bytes()
    if len(reference) != 47880 or len(actual) != len(reference):
        raise AssertionError('Incomplete convolution sample data')
    for index, (r, a) in enumerate(zip(struct.iter_unpack('>i', reference), struct.iter_unpack('>i', actual))):
        if abs(r[0] - a[0]) > 2:
            raise AssertionError(f'Convolution sample {index} differs: {r[0]} vs {a[0]}')
    print('Convolution samples match JDK within integer-kernel rounding', flush=True)


if __name__ == '__main__':
    main()
