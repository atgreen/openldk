// Test for method java.util.IllegalFormatFlagsException.getClass().getConstructors()

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

package gnu.testlet.java.util.IllegalFormatFlagsException.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.IllegalFormatFlagsException;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.util.IllegalFormatFlagsException.getClass().getConstructors()
 */
public class getConstructors implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // map of constructors which should exists
        Map<String, String> testedConstructors = null;

        // map of constructors for (Open)JDK6
        Map<String, String> testedConstructors_jdk6 = new HashMap<String, String>();

        // map of constructors for (Open)JDK7
        Map<String, String> testedConstructors_jdk7 = new HashMap<String, String>();

        // map for constructors declared in (Open)JDK6
        testedConstructors_jdk6.put("public java.util.IllegalFormatFlagsException(java.lang.String)", "java.util.IllegalFormatFlagsException");

        // map for constructors declared in (Open)JDK7
        testedConstructors_jdk7.put("public java.util.IllegalFormatFlagsException(java.lang.String)", "java.util.IllegalFormatFlagsException");

        // create instance of a class IllegalFormatFlagsException
        final Object o = new IllegalFormatFlagsException("IllegalFormatFlagsException");

        // get a runtime class of an object "o"
        final Class c = o.getClass();

        // get the right map containing constructor signatures
        testedConstructors = getJavaVersion() < 7 ? testedConstructors_jdk6 : testedConstructors_jdk7;

        // get all constructors for this class
        java.lang.reflect.Constructor[] constructors = c.getConstructors();

        // expected number of constructors
        final int expectedNumberOfConstructors = testedConstructors.size();

        // basic check for a number of constructors
        harness.check(constructors.length, expectedNumberOfConstructors);

        // check if all constructors exist
        for (java.lang.reflect.Constructor constructor : constructors) {
            // constructor name should consists of package name + class name
            String constructorName = constructor.getName();
            // modifiers + package + class name + parameter types
            String constructorString = constructor.toString().replaceAll(" native ", " ");
            harness.check(testedConstructors.containsKey(constructorString));
            harness.check(testedConstructors.get(constructorString), constructorName);
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

