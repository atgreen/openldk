// Test for method java.util.IllegalFormatPrecisionException.getClass().getFields()

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

package gnu.testlet.java.util.IllegalFormatPrecisionException.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.IllegalFormatPrecisionException;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.util.IllegalFormatPrecisionException.getClass().getFields()
 */
public class getFields implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // map of fields which should exists
        Map<String, String> testedFields = null;

        // map of fields for (Open)JDK6
        Map<String, String> testedFields_jdk6 = new HashMap<String, String>();

        // map of fields for (Open)JDK7
        Map<String, String> testedFields_jdk7 = new HashMap<String, String>();

        // map for fields declared in (Open)JDK6
        // --- empty ---

        // map for fields declared in (Open)JDK7
        // --- empty ---

        // create instance of a class IllegalFormatPrecisionException
        final Object o = new IllegalFormatPrecisionException(42);

        // get a runtime class of an object "o"
        final Class c = o.getClass();

        // get the right map containing field signatures
        testedFields = getJavaVersion() < 7 ? testedFields_jdk6 : testedFields_jdk7;

        // get all fields for this class
        java.lang.reflect.Field[] fields = c.getFields();

        // expected number of fields
        final int expectedNumberOfFields = testedFields.size();

        // basic check for a number of fields
        harness.check(fields.length, expectedNumberOfFields);

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

