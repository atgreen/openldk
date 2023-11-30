// Test if instances of a class java.lang.Error could be properly constructed

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

package gnu.testlet.java.lang.Error;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.Error;



/**
 * Test if instances of a class java.lang.Error
 * could be properly constructed
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
        Error error1 = new Error();
        harness.check(error1 != null);
        harness.check(error1.toString(), "java.lang.Error");

        Error error2 = new Error("nothing happens");
        harness.check(error2 != null);
        harness.check(error2.toString(), "java.lang.Error: nothing happens");

        Error error3 = new Error(new Throwable());
        harness.check(error3 != null);
        harness.check(error3.toString(), "java.lang.Error: java.lang.Throwable");

        Error error4 = new Error("nothing happens", new Throwable());
        harness.check(error4 != null);
        harness.check(error4.toString(), "java.lang.Error: nothing happens");
    }
}

