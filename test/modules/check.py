#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Compare the boot module layer and JDK package ownership on both runtimes."""
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
JDK = Path(os.environ['JAVA_HOME'])
BUILD = ROOT / '.cache/modules-test'
BUILD.mkdir(parents=True, exist_ok=True)
subprocess.run([str(JDK / 'bin/javac'), '-d', str(BUILD),
                str(ROOT / 'test/modules/ModuleLayerProbe.java')], check=True, timeout=60)
for engine in [JDK / 'bin/java', ROOT / 'openldk']:
    result = subprocess.run([str(engine), '-cp', str(BUILD), 'ModuleLayerProbe'],
                            cwd=ROOT, capture_output=True, text=True, timeout=180)
    (BUILD / (engine.name + '.log')).write_text(result.stdout + result.stderr)
    if result.returncode or 'Boot module layer, ownership, and exports passed' not in result.stdout:
        raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
    print(f'{engine.name}: {result.stdout.strip()}', flush=True)
