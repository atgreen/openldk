// Test for method java.lang.Class.isArray()

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

package gnu.testlet.java.lang.Class;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.Class;



/**
 * Test for method java.lang.Class.isArray()
 */
public class isArray implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // primitive types and it's corresponding classes
        checkClass(harness, "java.lang.Boolean", false);
        checkClass(harness, "java.lang.Character", false);
        checkClass(harness, "java.lang.Byte", false);
        checkClass(harness, "java.lang.Short", false);
        checkClass(harness, "java.lang.Integer", false);
        checkClass(harness, "java.lang.Long", false);
        checkClass(harness, "java.lang.Float", false);
        checkClass(harness, "java.lang.Double", false);
        checkClass(harness, "java.lang.Object", false);

        // one-dimensional arrays
        checkClass(harness, "[Z", true);
        checkClass(harness, "[C", true);
        checkClass(harness, "[B", true);
        checkClass(harness, "[S", true);
        checkClass(harness, "[I", true);
        checkClass(harness, "[J", true);
        checkClass(harness, "[F", true);
        checkClass(harness, "[D", true);
        checkClass(harness, "[Ljava.lang.Object;", true);

        // two-dimensional arrays
        checkClass(harness, "[[Z", true);
        checkClass(harness, "[[C", true);
        checkClass(harness, "[[B", true);
        checkClass(harness, "[[S", true);
        checkClass(harness, "[[I", true);
        checkClass(harness, "[[J", true);
        checkClass(harness, "[[F", true);
        checkClass(harness, "[[D", true);
        checkClass(harness, "[[Ljava.lang.Object;", true);

        // three-dimensional arrays
        checkClass(harness, "[[[Z", true);
        checkClass(harness, "[[[C", true);
        checkClass(harness, "[[[B", true);
        checkClass(harness, "[[[S", true);
        checkClass(harness, "[[[I", true);
        checkClass(harness, "[[[J", true);
        checkClass(harness, "[[[F", true);
        checkClass(harness, "[[[D", true);
        checkClass(harness, "[[[Ljava.lang.Object;", true);
    }

    public void checkClass(TestHarness harness, String className, boolean isArray)
    {
        try
        {
            Class c = Class.forName(className);
            harness.check(c.isArray() == isArray);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            harness.check(false);
        }
    }
}

