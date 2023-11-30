// Test for method java.lang.ArrayIndexOutOfBoundsException.getClass().getDeclaredField()

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

package gnu.testlet.java.lang.ArrayIndexOutOfBoundsException.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.ArrayIndexOutOfBoundsException;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.lang.ArrayIndexOutOfBoundsException.getClass().getDeclaredField()
 */
public class getDeclaredField implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // following declared fields should exists
        final String[] fieldsThatShouldExist_jdk6 = {
        };
        final String[] fieldsThatShouldExist_jdk7 = {
        };

        final String[] fieldsThatShouldExist = getJavaVersion() < 7 ? fieldsThatShouldExist_jdk6 : fieldsThatShouldExist_jdk7;

        // create instance of a class ArrayIndexOutOfBoundsException
        final Object o = new ArrayIndexOutOfBoundsException("java.lang.ArrayIndexOutOfBoundsException");

        // get a runtime class of an object "o"
        final Class c = o.getClass();

        // check if all required fields really exists
        for (String fieldThatShouldExists : fieldsThatShouldExist) {
            try {
                java.lang.reflect.Field field = c.getDeclaredField(fieldThatShouldExists);
                harness.check(field != null);
                String fieldName = field.getName();
                harness.check(fieldName != null);
                harness.check(fieldName, fieldThatShouldExists);
            }
            catch (Exception e) {
                harness.check(false);
            }
        }
    }

    /**
     * Returns version of Java. The input could have the following form: "1.7.0_06"
     * and we are interested only in "7" in this case.
     * 
     * @return Java version
     */
    protected int getJavaVersion() {
        String javaVersionStr = System.getProperty("java.version");
        String[] parts = javaVersionStr.split("\\.");
        return Integer.parseInt(parts[1]);
    }
}

