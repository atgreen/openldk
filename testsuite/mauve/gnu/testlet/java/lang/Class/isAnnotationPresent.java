// Test for method java.lang.Class.isAnnotationPresent()

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
 * Test for method java.lang.Class.isAnnotation()
 */
public class isAnnotationPresent implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // get a runtime class of an Class
        Class c1 = Class.class;
        harness.check(!c1.isAnnotationPresent(java.lang.annotation.Annotation.class));

        // get a runtime class of an Identify
        Class c2 = java.security.Identity.class;
        harness.check(!c2.isAnnotationPresent(java.lang.annotation.Annotation.class));
        harness.check(!c2.isAnnotationPresent(java.lang.annotation.Documented.class));
        // this class is deprecated!
        harness.check(c2.isAnnotationPresent(java.lang.Deprecated.class));
    }
}

