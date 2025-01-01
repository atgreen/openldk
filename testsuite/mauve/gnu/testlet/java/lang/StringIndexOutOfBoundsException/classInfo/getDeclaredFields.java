// Test for method java.lang.StringIndexOutOfBoundsException.getClass().getDeclaredFields()

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

// Tags: JDK1.5

package gnu.testlet.java.lang.StringIndexOutOfBoundsException.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.StringIndexOutOfBoundsException;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.lang.StringIndexOutOfBoundsException.getClass().getDeclaredFields()
 */
public class getDeclaredFields implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // map of declared fields which should exists
        Map<String, String> testedDeclaredFields = null;

        // map of declared fields for (Open)JDK6
        Map<String, String> testedDeclaredFields_jdk6 = new HashMap<String, String>();

        // map of declared fields for (Open)JDK7
        Map<String, String> testedDeclaredFields_jdk7 = new HashMap<String, String>();

        // map for fields declared in (Open)JDK6
        // --- empty ---

        // map for fields declared in (Open)JDK7
        testedDeclaredFields_jdk7.put("private static final long java.lang.StringIndexOutOfBoundsException.serialVersionUID", "serialVersionUID");

        // create instance of a class StringIndexOutOfBoundsException
        final Object o = new StringIndexOutOfBoundsException("java.lang.StringIndexOutOfBoundsException");

        // get a runtime class of an object "o"
        final Class c = o.getClass();

        // get the right map containing declared field signatures
        testedDeclaredFields = getJavaVersion() < 7 ? testedDeclaredFields_jdk6 : testedDeclaredFields_jdk7;

        // get all declared fields for this class
        java.lang.reflect.Field[] declaredFields = c.getDeclaredFields();

        // expected number of declared fields
        final int expectedNumberOfDeclaredFields = testedDeclaredFields.size();

        // basic check for a number of declared fields
        harness.check(declaredFields.length, expectedNumberOfDeclaredFields);

        // check if all fields exist
        for (java.lang.reflect.Field declaredField: declaredFields) {
            String fieldName = declaredField.getName();
            String fieldString = declaredField.toString();
            harness.check(testedDeclaredFields.containsKey(fieldString));
            harness.check(testedDeclaredFields.get(fieldString), fieldName);
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

