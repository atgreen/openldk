#!/usr/bin/env python3
# Copyright (C) 2026 Anthony Green <green@moxielogic.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Regression checks for legacy-test accounting, independent of the VM."""
import importlib.util
from pathlib import Path
import tempfile
import shutil
import subprocess
import unittest

spec = importlib.util.spec_from_file_location('audit', Path(__file__).with_name('audit-failures.py'))
audit = importlib.util.module_from_spec(spec)
spec.loader.exec_module(audit)


class AccountingTests(unittest.TestCase):
    def test_main_with_throws_and_modifiers(self):
        for signature in ['public static void main(java.lang.String[]) throws java.io.IOException;',
                          'public static final void main(java.lang.String[]);',
                          'public static void main(java.lang.String...);']:
            self.assertEqual('Actual', audit.find_main('class Actual {\n  ' + signature, 'Test'))
        self.assertIsNone(audit.find_main('interface cost {\n public int price();', 'Test'))
        self.assertIsNone(audit.find_main('class Helper {\n private static void main(java.lang.String[]);', 'Test'))

    def compare(self, reference, actual, golden, ref_status=1, actual_status=1):
        with tempfile.TemporaryDirectory() as directory:
            ref, out = Path(directory) / 'jdk.log', Path(directory) / 'openldk.log'
            ref.write_text(reference)
            out.write_text(actual)
            return audit.compare_legacy({'status': ref_status, 'log': str(ref)},
                                        {'status': actual_status, 'log': str(out)}, golden, True)

    def test_intentional_exception_ignores_only_diagnostics(self):
        jdk = 'Exception in thread "main" java.lang.ArrayIndexOutOfBoundsException: Index -2 out of bounds\n at Test.main(Test.java:3)'
        ldk = 'Unhandled Java exception:\njava.lang.ArrayIndexOutOfBoundsException: -2\n at Test.main(Unknown Source)'
        golden = 'java.lang.ArrayIndexOutOfBoundsException: -2\n at '
        self.assertEqual('stale-golden', self.compare(jdk, ldk, golden))
        self.assertEqual('runtime-failure', self.compare(jdk, ldk.replace('ArrayIndexOutOfBoundsException', 'NullPointerException'), golden))
        self.assertEqual('runtime-failure', self.compare(jdk, 'unexpected output\n' + ldk, golden))
        self.assertEqual('runtime-failure', self.compare(jdk, ldk, golden, actual_status='timeout'))

    def test_nonzero_exit_is_not_an_uncaught_exception(self):
        self.assertEqual('stale-pass', self.compare('3\n', '3\n', '3', 3, 3))
        self.assertEqual('runtime-failure', self.compare('3\n', '11\n', '3', 3, 11))
        self.assertEqual('runtime-failure', self.compare('3\n', '3\n', '3', 3, 0))
        self.assertEqual('runtime-failure', self.compare('new behavior\n', 'old behavior\n', 'old behavior', 0, 0))


    def test_reference_failure_is_never_a_mauve_pass(self):
        self.assertEqual('reference-failure', audit.classify_mauve({'status': 1}, {'status': 0}))
        self.assertEqual('reference-timeout', audit.classify_mauve({'status': 'timeout'}, {'status': 0}))
        self.assertEqual('runtime-failure', audit.classify_mauve({'status': 0}, {'status': 1}))
        self.assertEqual('stale-pass', audit.classify_mauve({'status': 0}, {'status': 0}))

    def test_gui_background_failures_are_not_mauve_passes(self):
        with tempfile.TemporaryDirectory() as directory:
            reference, actual = Path(directory) / 'jdk.log', Path(directory) / 'openldk.log'
            reference.write_text('Checks: 2, failures: 0\n')
            ref = {'status': 0, 'log': str(reference)}
            out = {'status': 0, 'log': str(actual)}
            for failure in [
                    'WARNING: Exception during peer initialization\njava.lang.UnsatisfiedLinkError: nativePeer\n',
                    'Exception during disposal:\njava.lang.NullPointerException\n',
                    'Thread #<java/awt/EventDispatchThread {123}> terminated with error: internal error\n',
                    'CORRUPTION WARNING in SBCL\n']:
                actual.write_text(failure + 'Checks: 2, failures: 0\n')
                self.assertEqual('runtime-failure', audit.classify_mauve(ref, out))

    def test_matching_intentional_gui_listener_exceptions_remain_passes(self):
        with tempfile.TemporaryDirectory() as directory:
            reference, actual = Path(directory) / 'jdk.log', Path(directory) / 'openldk.log'
            reference.write_text('Exception in thread "AWT-EventQueue-0" java.lang.IllegalStateException: expected\nChecks: 1, failures: 0\n')
            actual.write_text('Exception in thread "AWT-EventQueue-1"\njava.lang.IllegalStateException: expected\nChecks: 1, failures: 0\n')
            ref, out = {'status': 0, 'log': str(reference)}, {'status': 0, 'log': str(actual)}
            self.assertEqual('stale-pass', audit.classify_mauve(ref, out))
            actual.write_text(actual.read_text().replace('IllegalStateException', 'UnsatisfiedLinkError'))
            self.assertEqual('runtime-failure', audit.classify_mauve(ref, out))

    def test_obsolete_version_parser_is_not_a_reflection_failure(self):
        with tempfile.TemporaryDirectory() as directory:
            source = Path(directory) / 'Old.java'
            source.write_text('System.getProperty("java.version"); Integer.parseInt(parts[1]);')
            result = audit.classify_obsolete_source({'compile': {'status': 0}, 'classification': 'runtime-failure'}, source)
            self.assertEqual('obsolete-version-parser', result['classification'])
            source.write_text('Runtime.version().feature();')
            result = audit.classify_obsolete_source({'compile': {'status': 0}, 'classification': 'runtime-failure'}, source)
            self.assertEqual('runtime-failure', result['classification'])

    @unittest.skipUnless(shutil.which('expect'), 'DejaGnu/expect is unavailable')
    def test_exact_tcl_expectation_lookup(self):
        library = Path(__file__).with_name('lib').joinpath('openldk.exp').resolve()
        with tempfile.TemporaryDirectory() as directory:
            expected = Path(directory) / 'expected-failures.txt'
            expected.write_text('# comment\nXFAIL: gcj/Foo.java execution test\n')
            (Path(directory) / 'unsupported-tests.txt').write_text('gcj/Old.java\tcompile-rejected\tobsolete source\n')
            command = 'source {' + str(library) + '}; set testname gcj/Wrong.java; puts [list [is_expected_failure gcj/Foo.java] [is_expected_failure gcj/foo.java] [is_expected_failure gcj/FooXjava]]; puts [unsupported_test_reason gcj/Old.java]'
            result = subprocess.run(['expect', '-c', command], cwd=directory, capture_output=True, text=True, check=True)
            self.assertEqual(['1 0 0', 'compile-rejected {obsolete source}'], result.stdout.strip().splitlines())
            for invalid in ['2025-03-04 FAIL: gcj/Foo.java execution test\n',
                            'XFAIL: gcj/Foo.java execution test\nXFAIL: gcj/Foo.java execution test\n']:
                expected.write_text(invalid)
                command = 'source {' + str(library) + '}; if {![catch {is_expected_failure gcj/Foo.java} problem]} {exit 1}; puts $problem'
                result = subprocess.run(['expect', '-c', command], cwd=directory, capture_output=True, text=True, check=True)
                self.assertRegex(result.stdout, 'Malformed|Duplicate')


if __name__ == '__main__':
    unittest.main()
