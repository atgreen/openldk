// Test if instances of a class java.lang.ArrayIndexOutOfBoundsException could be properly constructed

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

package gnu.testlet.java.lang.ArrayIndexOutOfBoundsException;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.ArrayIndexOutOfBoundsException;



/**
 * Test if instances of a class java.lang.ArrayIndexOutOfBoundsException
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
        ArrayIndexOutOfBoundsException object1 = new ArrayIndexOutOfBoundsException();
        harness.check(object1 != null);
        harness.check(object1.toString(), "java.lang.ArrayIndexOutOfBoundsException");

        ArrayIndexOutOfBoundsException object2 = new ArrayIndexOutOfBoundsException("nothing happens");
        harness.check(object2 != null);
        harness.check(object2.toString(), "java.lang.ArrayIndexOutOfBoundsException: nothing happens");

        ArrayIndexOutOfBoundsException object3 = new ArrayIndexOutOfBoundsException(null);
        harness.check(object3 != null);
        harness.check(object3.toString(), "java.lang.ArrayIndexOutOfBoundsException");

        ArrayIndexOutOfBoundsException object4 = new ArrayIndexOutOfBoundsException(0);
        harness.check(object4 != null);
        harness.check(object4.toString(), "java.lang.ArrayIndexOutOfBoundsException: Array index out of range: 0");

        ArrayIndexOutOfBoundsException object5 = new ArrayIndexOutOfBoundsException(-1);
        harness.check(object5 != null);
        harness.check(object5.toString(), "java.lang.ArrayIndexOutOfBoundsException: Array index out of range: -1");

        ArrayIndexOutOfBoundsException object6 = new ArrayIndexOutOfBoundsException(Integer.MAX_VALUE);
        harness.check(object6 != null);
        harness.check(object6.toString(), "java.lang.ArrayIndexOutOfBoundsException: Array index out of range: 2147483647");

    }
}

