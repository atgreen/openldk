// Test for method java.lang.AbstractMethodError.getClass().getDeclaredAnnotations()

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

package gnu.testlet.java.lang.AbstractMethodError.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.AbstractMethodError;
import java.util.List;
import java.util.Arrays;



/**
 * Test for method java.lang.AbstractMethodError.getClass().getDeclaredAnnotations()
 */
public class getDeclaredAnnotations implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // create instance of a class AbstractMethodError
        final Object o = new AbstractMethodError("AbstractMethodError");

        // get a runtime class of an object "o"
        final Class c = o.getClass();

        // following annotations should be provided
        final String[] annotationsThatShouldExists_jdk6 = {
        };

        final String[] annotationsThatShouldExists_jdk7 = {
        };

        // get all annotations
        java.lang.annotation.Annotation[] annotations = c.getDeclaredAnnotations();

        // and transform the array into a list of annotation names
        List<String> annotationsAsString = new java.util.ArrayList<String>();

        for (java.lang.annotation.Annotation annotation : annotations) {
            annotationsAsString.add(annotation.toString());
        }

        String[] annotationsThatShouldExists = getJavaVersion() < 7 ? annotationsThatShouldExists_jdk6 : annotationsThatShouldExists_jdk7;

        // check if all required annotations really exist
        for (String annotationThatShouldExists : annotationsThatShouldExists) {
            harness.check(annotationsAsString.contains(annotationThatShouldExists));
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

