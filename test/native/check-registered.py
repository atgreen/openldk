#!/usr/bin/env python3
# Copyright (C) 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Exercise registered CL bodies for Java native declarations and bytecode overrides."""
import os
from pathlib import Path
import subprocess
ROOT = Path(__file__).resolve().parents[2]
JDK = Path(os.environ['JAVA_HOME'])
BUILD = ROOT / '.cache/registered-native-test'
BUILD.mkdir(parents=True, exist_ok=True)
subprocess.run([str(JDK / 'bin/javac'), '-d', str(BUILD),
                str(ROOT / 'test/native/RegisteredNativeProbe.java')], check=True, timeout=60)
environment = dict(os.environ, LDK_REGISTERED_NATIVE_DIR=str(BUILD))
result = subprocess.run(['sbcl', '--dynamic-space-size', '8192', '--non-interactive',
                         '--eval', '(progn (push (uiop:getcwd) asdf:*central-registry*) '
                                   '(asdf:load-system :openldk))',
                         '--load', str(ROOT / 'test/native/registered.lisp')],
                        cwd=ROOT, env=environment, capture_output=True, text=True, timeout=300)
(BUILD / 'registered.log').write_text(result.stdout + result.stderr)
if result.returncode or 'Registered CL static, instance, private, and bytecode bodies passed' not in result.stdout:
    raise RuntimeError(result.stdout + result.stderr)
print('Registered CL static, instance, private, and bytecode bodies passed')
