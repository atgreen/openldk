#!/usr/bin/env python3
# Copyright (C) 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Check class-definition publication with deterministically paused writers."""
import os
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
JDK = Path(os.environ['JAVA_HOME'])
with tempfile.TemporaryDirectory(prefix='openldk-publication-') as temporary:
    directory = Path(temporary)
    source = directory / 'PublicationSetup.java'
    source.write_text('class PublicationSetup { public static void main(String[] args) {} }\n'
                      'class PublicationSuccess { static int value = 42; }\n'
                      'class PublicationFailure { static int value = 42; }\n'
                      'class PublicationMirrorSuccess { static int value = 42; }\n'
                      'class PublicationMirrorFailure { static int value = 42; }\n'
                      'class PublicationCloseFailure { static int value = 42; }\n')
    subprocess.run([str(JDK / 'bin/javac'), '-d', str(directory), str(source)],
                   check=True, timeout=60)
    environment = dict(os.environ, LDK_PUBLICATION_TEST_DIR=str(directory),
                       CL_SOURCE_REGISTRY=str(ROOT) + '/:')
    subprocess.run(['sbcl', '--dynamic-space-size', '8192', '--non-interactive',
                    '--load', str(ROOT / 'test/concurrency/publication.lisp')],
                   cwd=ROOT, env=environment, check=True, timeout=240)
