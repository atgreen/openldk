// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
public class AssertionProbe {
    public static class Pass implements Testlet {
        public void test(TestHarness harness) {
            harness.check(true); harness.check(1, 1);
            harness.checkPoint("again");
            harness.check("same", "same"); harness.check(true);
            harness.debug("Debug marker");
        }
    }
    public static class Fail implements Testlet {
        public void test(TestHarness harness) {
            harness.check(true); harness.check(false);
            harness.checkPoint("again"); harness.check(false);
        }
    }
    public static class Crash implements Testlet {
        public void test(TestHarness harness) {
            harness.check(true);
            throw new IllegalStateException("expected exception");
        }
    }
}
