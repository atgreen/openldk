// Test for method java.lang.Class.isPrimitive()

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
 * Test for method java.lang.Class.isPrimitive()
 */
public class isPrimitive implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // primitive types
        harness.check(Boolean.TYPE.isPrimitive(), true);
        harness.check(Character.TYPE.isPrimitive(), true);
        harness.check(Byte.TYPE.isPrimitive(), true);
        harness.check(Short.TYPE.isPrimitive(), true);
        harness.check(Integer.TYPE.isPrimitive(), true);
        harness.check(Long.TYPE.isPrimitive(), true);
        harness.check(Float.TYPE.isPrimitive(), true);
        harness.check(Double.TYPE.isPrimitive(), true);
        harness.check(Void.TYPE.isPrimitive(), true);

        // classes corresponding to primitive types
        checkClass(harness, "java.lang.Boolean", false);
        checkClass(harness, "java.lang.Character", false);
        checkClass(harness, "java.lang.Byte", false);
        checkClass(harness, "java.lang.Short", false);
        checkClass(harness, "java.lang.Integer", false);
        checkClass(harness, "java.lang.Long", false);
        checkClass(harness, "java.lang.Float", false);
        checkClass(harness, "java.lang.Double", false);

        // common object
        checkClass(harness, "java.lang.Object", false);

        // one-dimensional arrays
        checkClass(harness, "[Z", false);
        checkClass(harness, "[C", false);
        checkClass(harness, "[B", false);
        checkClass(harness, "[S", false);
        checkClass(harness, "[I", false);
        checkClass(harness, "[J", false);
        checkClass(harness, "[F", false);
        checkClass(harness, "[D", false);
        checkClass(harness, "[Ljava.lang.Object;", false);

        // two-dimensional arrays
        checkClass(harness, "[[Z", false);
        checkClass(harness, "[[C", false);
        checkClass(harness, "[[B", false);
        checkClass(harness, "[[S", false);
        checkClass(harness, "[[I", false);
        checkClass(harness, "[[J", false);
        checkClass(harness, "[[F", false);
        checkClass(harness, "[[D", false);
        checkClass(harness, "[[Ljava.lang.Object;", false);

        // three-dimensional arrays
        checkClass(harness, "[[[Z", false);
        checkClass(harness, "[[[C", false);
        checkClass(harness, "[[[B", false);
        checkClass(harness, "[[[S", false);
        checkClass(harness, "[[[I", false);
        checkClass(harness, "[[[J", false);
        checkClass(harness, "[[[F", false);
        checkClass(harness, "[[[D", false);
        checkClass(harness, "[[[Ljava.lang.Object;", false);
    }

    public void checkClass(TestHarness harness, String className, boolean isPrimitive)
    {
        try
        {
            Class c = Class.forName(className);
            harness.check(c.isPrimitive() == isPrimitive);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            harness.check(false);
        }
    }
}

