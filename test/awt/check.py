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
                    str(ROOT / 'test/awt/BufferedImageProbe.java')], check=True, timeout=60)
    for engine in [JDK / 'bin/java', options.runtime.resolve()]:
        result = subprocess.run([str(engine), '-Djava.awt.headless=true', '-cp',
                                 str(BUILD), 'BufferedImageProbe'], cwd=ROOT,
                                capture_output=True, text=True, timeout=120)
        (BUILD / (engine.name + '.log')).write_text(result.stdout + result.stderr)
        if result.returncode or MARKER not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)


if __name__ == '__main__':
    main()
