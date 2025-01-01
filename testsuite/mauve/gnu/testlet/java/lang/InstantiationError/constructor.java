// Test if instances of a class java.lang.InstantiationError could be properly constructed

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

package gnu.testlet.java.lang.InstantiationError;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.InstantiationError;



/**
 * Test if instances of a class java.lang.InstantiationError
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
        InstantiationError object1 = new InstantiationError();
        harness.check(object1 != null);
        harness.check(object1.toString(), "java.lang.InstantiationError");

        InstantiationError object2 = new InstantiationError("nothing happens");
        harness.check(object2 != null);
        harness.check(object2.toString(), "java.lang.InstantiationError: nothing happens");

        InstantiationError object3 = new InstantiationError(null);
        harness.check(object3 != null);
        harness.check(object3.toString(), "java.lang.InstantiationError");

    }
}

