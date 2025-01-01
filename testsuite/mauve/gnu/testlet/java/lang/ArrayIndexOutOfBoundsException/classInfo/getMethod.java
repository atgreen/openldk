// Test for method java.lang.ArrayIndexOutOfBoundsException.getClass().getMethod()

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
 * Test for method java.lang.ArrayIndexOutOfBoundsException.getClass().getMethod()
 */
public class getMethod implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // following methods should exist
        Map<String, Class[]> methodsThatShouldExist_jdk6 = new HashMap<String, Class[]>();
        methodsThatShouldExist_jdk6.put("printStackTrace", new Class[] {});
        methodsThatShouldExist_jdk6.put("printStackTrace", new Class[] {java.io.PrintStream.class});
        methodsThatShouldExist_jdk6.put("printStackTrace", new Class[] {java.io.PrintWriter.class});
        methodsThatShouldExist_jdk6.put("fillInStackTrace", new Class[] {});
        methodsThatShouldExist_jdk6.put("getCause", new Class[] {});
        methodsThatShouldExist_jdk6.put("initCause", new Class[] {java.lang.Throwable.class});
        methodsThatShouldExist_jdk6.put("toString", new Class[] {});
        methodsThatShouldExist_jdk6.put("getMessage", new Class[] {});
        methodsThatShouldExist_jdk6.put("getLocalizedMessage", new Class[] {});
        methodsThatShouldExist_jdk6.put("getStackTrace", new Class[] {});
        methodsThatShouldExist_jdk6.put("setStackTrace", new Class[] {new java.lang.StackTraceElement[0].getClass()});
        methodsThatShouldExist_jdk6.put("wait", new Class[] {long.class});
        methodsThatShouldExist_jdk6.put("wait", new Class[] {long.class, int.class});
        methodsThatShouldExist_jdk6.put("wait", new Class[] {});
        methodsThatShouldExist_jdk6.put("hashCode", new Class[] {});
        methodsThatShouldExist_jdk6.put("getClass", new Class[] {});
        methodsThatShouldExist_jdk6.put("equals", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk6.put("notify", new Class[] {});
        methodsThatShouldExist_jdk6.put("notifyAll", new Class[] {});

        Map<String, Class[]> methodsThatShouldExist_jdk7 = new HashMap<String, Class[]>();

        // get the right map containing method signatures
        Map<String, Class[]> methodsThatShouldExist = getJavaVersion() < 7 ? methodsThatShouldExist_jdk6 : methodsThatShouldExist_jdk7;

        // create instance of a class ArrayIndexOutOfBoundsException
        final Object o = new ArrayIndexOutOfBoundsException("java.lang.ArrayIndexOutOfBoundsException");

        // get a runtime class of an object "o"
        final Class c = o.getClass();

        // check if all required methods really exist
        for (Map.Entry<String, Class[]> methodThatShouldExists : methodsThatShouldExist.entrySet()) {
            try {
                java.lang.reflect.Method method = c.getMethod(methodThatShouldExists.getKey(), methodThatShouldExists.getValue());
                harness.check(method != null);
                String methodName = method.getName();
                harness.check(methodName != null);
                harness.check(methodName, methodThatShouldExists.getKey());
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

