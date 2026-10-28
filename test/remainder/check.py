#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Generate remainder vectors with JDK 25 and execute identical bytecode on both VMs."""
import argparse
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
JDK = Path(os.environ['JAVA_HOME'])
BUILD = ROOT / '.cache/remainder-test'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    args = parser.parse_args()
    BUILD.mkdir(parents=True, exist_ok=True)
    source = ROOT / 'test/remainder/RemainderProbe.java'
    subprocess.run([str(JDK / 'bin/javac'), '--release', '25', '-g', '-d', str(BUILD), str(source)], check=True, timeout=60)
    vectors = BUILD / 'jdk-vectors.bin'
    subprocess.run([str(JDK / 'bin/java'), '-cp', str(BUILD), 'RemainderProbe', 'generate', str(vectors)], check=True, timeout=60)
    for engine in [JDK / 'bin/java', args.runtime.resolve()]:
        result = subprocess.run([str(engine), '-cp', str(BUILD), 'RemainderProbe', 'verify', str(vectors)],
                                text=True, capture_output=True, timeout=240)
        output = result.stdout + result.stderr
        (BUILD / (engine.name + '.log')).write_text(output)
        if result.returncode or 'JDK vectors passed' not in result.stdout or 'terminated with error' in output:
            raise RuntimeError(f'{engine}:\n{output}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)


if __name__ == '__main__':
    main()
