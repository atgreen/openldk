// Test if try-catch block is working properly for a class java.lang.ArithmeticException

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

package gnu.testlet.java.lang.ArithmeticException;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.ArithmeticException;



/**
 * Test if try-catch block is working properly for a class java.lang.ArithmeticException
 */
public class TryCatch implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // flag that is set when exception is caught
        boolean caught = false;
        try {
            throw new ArithmeticException("ArithmeticException");
        }
        catch (ArithmeticException e) {
            // correct exception was caught
            caught = true;
        }
        harness.check(caught);
    }
}

