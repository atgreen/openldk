#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Compare live thread-management behavior with JDK 25."""
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
JDK = Path(os.environ['JAVA_HOME'])
BUILD = ROOT / '.cache/management-test'
BUILD.mkdir(parents=True, exist_ok=True)
subprocess.run([str(JDK / 'bin/javac'), '-d', str(BUILD),
                str(ROOT / 'test/management/ThreadInfoProbe.java')], check=True, timeout=60)
for engine in [JDK / 'bin/java', ROOT / 'openldk']:
    result = subprocess.run([str(engine), '-cp', str(BUILD), 'ThreadInfoProbe'],
                            capture_output=True, text=True, timeout=120)
    (BUILD / (engine.name + '.log')).write_text(result.stdout + result.stderr)
    marker = 'Thread management snapshots, stacks, states, locks, depth limits, and dumps passed'
    if result.returncode or marker not in result.stdout or 'terminated with error' in result.stderr:
        raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
    print(f'{engine.name}: {result.stdout.strip()}', flush=True)
