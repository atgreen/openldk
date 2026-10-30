#!/usr/bin/env python3
# Copyright (C) 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Check that saved images retain file modes, positions, sharing and writes."""
import os
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
JDK = Path(os.environ['JAVA_HOME'])
MARKER = 'File-output image flags, positions, sharing and visibility passed'


def check(command, **kwargs):
    result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, **kwargs)
    if result.returncode or MARKER not in result.stdout:
        raise AssertionError(f'{command}:\n{result.stdout}\n{result.stderr}')


def main():
    with tempfile.TemporaryDirectory(prefix='output-image-', dir=ROOT / '.cache') as temporary:
        directory = Path(temporary)
        subprocess.run([str(JDK / 'bin/javac'), '-g', '-d', str(directory),
                        str(ROOT / 'test/nio/FileOutputImage.java')], check=True, timeout=60)
        check([str(JDK / 'bin/java'), '-cp', str(directory), 'FileOutputImage',
               'roundtrip', str(directory / 'reference')], timeout=120)
        print('java: file-output reference passed', flush=True)
        env = dict(os.environ, LDK_OUTPUT_IMAGE_DIR=str(directory),
                   CL_SOURCE_REGISTRY=str(ROOT) + '/:')
        with (directory / 'build.log').open('w') as log:
            result = subprocess.run(['sbcl', '--dynamic-space-size', '8192', '--non-interactive',
                                     '--eval', '(require :asdf)',
                                     '--eval', '(push (uiop:getcwd) asdf:*central-registry*)',
                                     '--eval', '(asdf:load-system :openldk)',
                                     '--load', str(ROOT / 'test/nio/output-image.lisp')],
                                    cwd=ROOT, env=env, stdout=log, stderr=subprocess.STDOUT,
                                    timeout=600)
        if result.returncode:
            raise AssertionError((directory / 'build.log').read_text()[-16000:])
        check([str(directory / 'output-image')], timeout=240)
        print('openldk: saved-image file-output reference passed', flush=True)


if __name__ == '__main__':
    main()
