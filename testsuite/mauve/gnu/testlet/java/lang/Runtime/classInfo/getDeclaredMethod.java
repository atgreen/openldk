// Test for method java.lang.Runtime.getClass().getDeclaredMethod()

// Copyright (C) 2012, 2013, 2014, 2015 Pavel Tisnovsky <ptisnovs@redhat.com>

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

package gnu.testlet.java.lang.Runtime.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.Runtime;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.lang.Runtime.getClass().getDeclaredMethod()
 */
public class getDeclaredMethod implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // following declared methods should exist
        Map<String, Class[]> methodsThatShouldExist_jdk6 = new HashMap<String, Class[]>();
        methodsThatShouldExist_jdk6.put("exit", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("runFinalization", new Class[] {});
        methodsThatShouldExist_jdk6.put("runFinalizersOnExit", new Class[] {boolean.class});
        methodsThatShouldExist_jdk6.put("loadLibrary", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("loadLibrary0", new Class[] {java.lang.Class.class, java.lang.String.class});
        methodsThatShouldExist_jdk6.put("load", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("gc", new Class[] {});
        methodsThatShouldExist_jdk6.put("getRuntime", new Class[] {});
        methodsThatShouldExist_jdk6.put("load0", new Class[] {java.lang.Class.class, java.lang.String.class});
        methodsThatShouldExist_jdk6.put("freeMemory", new Class[] {});
        methodsThatShouldExist_jdk6.put("maxMemory", new Class[] {});
        methodsThatShouldExist_jdk6.put("runFinalization0", new Class[] {});
        methodsThatShouldExist_jdk6.put("totalMemory", new Class[] {});
        methodsThatShouldExist_jdk6.put("addShutdownHook", new Class[] {java.lang.Thread.class});
        methodsThatShouldExist_jdk6.put("removeShutdownHook", new Class[] {java.lang.Thread.class});
        methodsThatShouldExist_jdk6.put("halt", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("exec", new Class[] {new java.lang.String[0].getClass()});
        methodsThatShouldExist_jdk6.put("exec", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("exec", new Class[] {java.lang.String.class, new java.lang.String[0].getClass(), java.io.File.class});
        methodsThatShouldExist_jdk6.put("exec", new Class[] {java.lang.String.class, new java.lang.String[0].getClass()});
        methodsThatShouldExist_jdk6.put("exec", new Class[] {new java.lang.String[0].getClass(), new java.lang.String[0].getClass(), java.io.File.class});
        methodsThatShouldExist_jdk6.put("exec", new Class[] {new java.lang.String[0].getClass(), new java.lang.String[0].getClass()});
        methodsThatShouldExist_jdk6.put("availableProcessors", new Class[] {});
        methodsThatShouldExist_jdk6.put("traceInstructions", new Class[] {boolean.class});
        methodsThatShouldExist_jdk6.put("traceMethodCalls", new Class[] {boolean.class});
        methodsThatShouldExist_jdk6.put("getLocalizedInputStream", new Class[] {java.io.InputStream.class});
        methodsThatShouldExist_jdk6.put("getLocalizedOutputStream", new Class[] {java.io.OutputStream.class});

        Map<String, Class[]> methodsThatShouldExist_jdk7 = new HashMap<String, Class[]>();
        methodsThatShouldExist_jdk7.put("exit", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("runFinalization", new Class[] {});
        methodsThatShouldExist_jdk7.put("runFinalizersOnExit", new Class[] {boolean.class});
        methodsThatShouldExist_jdk7.put("load", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("loadLibrary", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("loadLibrary0", new Class[] {java.lang.Class.class, java.lang.String.class});
        methodsThatShouldExist_jdk7.put("gc", new Class[] {});
        methodsThatShouldExist_jdk7.put("getRuntime", new Class[] {});
        methodsThatShouldExist_jdk7.put("load0", new Class[] {java.lang.Class.class, java.lang.String.class});
        methodsThatShouldExist_jdk7.put("freeMemory", new Class[] {});
        methodsThatShouldExist_jdk7.put("exec", new Class[] {new java.lang.String[0].getClass()});
        methodsThatShouldExist_jdk7.put("exec", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("exec", new Class[] {java.lang.String.class, new java.lang.String[0].getClass()});
        methodsThatShouldExist_jdk7.put("exec", new Class[] {new java.lang.String[0].getClass(), new java.lang.String[0].getClass(), java.io.File.class});
        methodsThatShouldExist_jdk7.put("exec", new Class[] {java.lang.String.class, new java.lang.String[0].getClass(), java.io.File.class});
        methodsThatShouldExist_jdk7.put("exec", new Class[] {new java.lang.String[0].getClass(), new java.lang.String[0].getClass()});
        methodsThatShouldExist_jdk7.put("maxMemory", new Class[] {});
        methodsThatShouldExist_jdk7.put("addShutdownHook", new Class[] {java.lang.Thread.class});
        methodsThatShouldExist_jdk7.put("availableProcessors", new Class[] {});
        methodsThatShouldExist_jdk7.put("getLocalizedInputStream", new Class[] {java.io.InputStream.class});
        methodsThatShouldExist_jdk7.put("getLocalizedOutputStream", new Class[] {java.io.OutputStream.class});
        methodsThatShouldExist_jdk7.put("halt", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("removeShutdownHook", new Class[] {java.lang.Thread.class});
        methodsThatShouldExist_jdk7.put("runFinalization0", new Class[] {});
        methodsThatShouldExist_jdk7.put("totalMemory", new Class[] {});
        methodsThatShouldExist_jdk7.put("traceInstructions", new Class[] {boolean.class});
        methodsThatShouldExist_jdk7.put("traceMethodCalls", new Class[] {boolean.class});

        // get the right map containing method signatures
        Map<String, Class[]> methodsThatShouldExist = getJavaVersion() < 7 ? methodsThatShouldExist_jdk6 : methodsThatShouldExist_jdk7;

        // create instance of a class Runtime
        final Object o = Runtime.getRuntime();

        // get a runtime class of an object "o"
        final Class c = o.getClass();

        // check if all required methods really exist
        for (Map.Entry<String, Class[]> methodThatShouldExists : methodsThatShouldExist.entrySet()) {
            try {
                java.lang.reflect.Method method = c.getDeclaredMethod(methodThatShouldExists.getKey(), methodThatShouldExists.getValue());
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

