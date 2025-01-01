// Test if instances of a class java.io.NotActiveException could be properly constructed

// Copyright (C) 2012 Pavel Tisnovsky <ptisnovs@redhat.com>

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

package gnu.testlet.java.io.NotActiveException;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.NotActiveException;



/**
 * Test if instances of a class java.io.NotActiveException
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
        NotActiveException object1 = new NotActiveException();
        harness.check(object1 != null);
        harness.check(object1.toString(), "java.io.NotActiveException");

        NotActiveException object2 = new NotActiveException("nothing happens");
        harness.check(object2 != null);
        harness.check(object2.toString(), "java.io.NotActiveException: nothing happens");

        NotActiveException object3 = new NotActiveException(null);
        harness.check(object3 != null);
        harness.check(object3.toString(), "java.io.NotActiveException");

    }
}

