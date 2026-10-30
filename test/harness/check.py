#!/usr/bin/env python3
# Copyright (C) 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Check that quiet Mauve reporting preserves assertions, failures, and exceptions."""
import os
from pathlib import Path
import subprocess
ROOT = Path(__file__).resolve().parents[2]
JDK = Path(os.environ['JAVA_HOME'])
BUILD = ROOT / '.cache/harness-test'
BUILD.mkdir(parents=True, exist_ok=True)
subprocess.run([str(JDK / 'bin/javac'), '-d', str(BUILD), '-sourcepath',
                str(ROOT / 'testsuite/mauve'), str(ROOT / 'testsuite/mauve/openldkHarness.java'),
                str(ROOT / 'test/harness/AssertionProbe.java')], check=True, timeout=60)
for engine in [JDK / 'bin/java', ROOT / 'openldk']:
    for mode in [[], ['--quiet-passes']]:
        for case in ['Pass', 'Fail', 'Crash']:
            result = subprocess.run([str(engine), '-cp', str(BUILD), 'mauve.openldkHarness',
                                     'AssertionProbe$' + case] + mode,
                                    capture_output=True, text=True, timeout=120)
            output = result.stdout + result.stderr
            if case == 'Pass':
                assert result.returncode == 0, output
                assert 'Debug marker' in output, output
                if mode:
                    assert 'Checks: 4, failures: 0' in output and 'PASS:' not in output, output
                else:
                    assert output.count('PASS:') == 4, output
            elif case == 'Fail':
                assert result.returncode != 0 and output.count('FAIL:') == 2, output
                assert 'again (number 0)' in output and '(number 1)' in output, output
                if mode:
                    assert 'Checks: 3, failures: 2' in output and 'PASS:' not in output, output
            else:
                assert result.returncode != 0 and 'java.lang.IllegalStateException' in output, output
            print(f'{engine.name} {case} quiet={bool(mode)}: PASS', flush=True)
