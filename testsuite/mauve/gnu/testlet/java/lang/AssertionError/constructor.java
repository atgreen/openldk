// Test if constructor is working properly for a class java.lang.AssertionError

// Copyright (C) 2012, 2013 Pavel Tisnovsky <ptisnovs@redhat.com>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 51 Franklin Street,
// Fifth Floor, Boston, MA 02110-1301 USA.

// Tags: JDK1.5

package gnu.testlet.java.lang.AssertionError;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.AssertionError;



/**
 * Test if constructor is working properly for a class java.lang.AssertionError
 */
public class constructor implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        AssertionError error1 = new AssertionError();
        harness.check(error1 != null);
        harness.check(error1.toString(), "java.lang.AssertionError");

        AssertionError error2 = new AssertionError(true);
        harness.check(error2 != null);
        harness.check(error2.toString(), "java.lang.AssertionError: true");

        AssertionError error3 = new AssertionError(false);
        harness.check(error3 != null);
        harness.check(error3.toString(), "java.lang.AssertionError: false");

        AssertionError error4 = new AssertionError('*');
        harness.check(error4 != null);
        harness.check(error4.toString(), "java.lang.AssertionError: *");

        AssertionError error5 = new AssertionError(42);
        harness.check(error5 != null);
        harness.check(error5.toString(), "java.lang.AssertionError: 42");

        AssertionError error6 = new AssertionError(42L);
        harness.check(error6 != null);
        harness.check(error6.toString(), "java.lang.AssertionError: 42");

        AssertionError error7 = new AssertionError(42.0);
        harness.check(error7 != null);
        harness.check(error7.toString(), "java.lang.AssertionError: 42.0");

        AssertionError error8 = new AssertionError(42.0f);
        harness.check(error8 != null);
        harness.check(error8.toString(), "java.lang.AssertionError: 42.0");

        AssertionError error9 = new AssertionError("xyzzy");
        harness.check(error9 != null);
        harness.check(error9.toString(), "java.lang.AssertionError: xyzzy");
    }
}

