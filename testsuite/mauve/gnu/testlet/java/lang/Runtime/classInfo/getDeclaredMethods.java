// Test for method java.lang.Runtime.getClass().getDeclaredMethods()

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
 * Test for method java.lang.Runtime.getClass().getDeclaredMethods()
 */
public class getDeclaredMethods implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // map of declared methods which should exists
        Map<String, String> testedDeclaredMethods = null;

        // map of declared methods for (Open)JDK6
        Map<String, String> testedDeclaredMethods_jdk6 = new HashMap<String, String>();

        // map of declared methods for (Open)JDK7
        Map<String, String> testedDeclaredMethods_jdk7 = new HashMap<String, String>();

        // map for methods declared in (Open)JDK6
        testedDeclaredMethods_jdk6.put("public void java.lang.Runtime.exit(int)", "exit");
        testedDeclaredMethods_jdk6.put("public void java.lang.Runtime.runFinalization()", "runFinalization");
        testedDeclaredMethods_jdk6.put("public static void java.lang.Runtime.runFinalizersOnExit(boolean)", "runFinalizersOnExit");
        testedDeclaredMethods_jdk6.put("public void java.lang.Runtime.loadLibrary(java.lang.String)", "loadLibrary");
        testedDeclaredMethods_jdk6.put("synchronized void java.lang.Runtime.loadLibrary0(java.lang.Class,java.lang.String)", "loadLibrary0");
        testedDeclaredMethods_jdk6.put("public void java.lang.Runtime.load(java.lang.String)", "load");
        testedDeclaredMethods_jdk6.put("public void java.lang.Runtime.gc()", "gc");
        testedDeclaredMethods_jdk6.put("public static java.lang.Runtime java.lang.Runtime.getRuntime()", "getRuntime");
        testedDeclaredMethods_jdk6.put("synchronized void java.lang.Runtime.load0(java.lang.Class,java.lang.String)", "load0");
        testedDeclaredMethods_jdk6.put("public long java.lang.Runtime.freeMemory()", "freeMemory");
        testedDeclaredMethods_jdk6.put("public long java.lang.Runtime.maxMemory()", "maxMemory");
        testedDeclaredMethods_jdk6.put("private static void java.lang.Runtime.runFinalization0()", "runFinalization0");
        testedDeclaredMethods_jdk6.put("public long java.lang.Runtime.totalMemory()", "totalMemory");
        testedDeclaredMethods_jdk6.put("public void java.lang.Runtime.addShutdownHook(java.lang.Thread)", "addShutdownHook");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Runtime.removeShutdownHook(java.lang.Thread)", "removeShutdownHook");
        testedDeclaredMethods_jdk6.put("public void java.lang.Runtime.halt(int)", "halt");
        testedDeclaredMethods_jdk6.put("public java.lang.Process java.lang.Runtime.exec(java.lang.String[]) throws java.io.IOException", "exec");
        testedDeclaredMethods_jdk6.put("public java.lang.Process java.lang.Runtime.exec(java.lang.String) throws java.io.IOException", "exec");
        testedDeclaredMethods_jdk6.put("public java.lang.Process java.lang.Runtime.exec(java.lang.String,java.lang.String[],java.io.File) throws java.io.IOException", "exec");
        testedDeclaredMethods_jdk6.put("public java.lang.Process java.lang.Runtime.exec(java.lang.String,java.lang.String[]) throws java.io.IOException", "exec");
        testedDeclaredMethods_jdk6.put("public java.lang.Process java.lang.Runtime.exec(java.lang.String[],java.lang.String[],java.io.File) throws java.io.IOException", "exec");
        testedDeclaredMethods_jdk6.put("public java.lang.Process java.lang.Runtime.exec(java.lang.String[],java.lang.String[]) throws java.io.IOException", "exec");
        testedDeclaredMethods_jdk6.put("public int java.lang.Runtime.availableProcessors()", "availableProcessors");
        testedDeclaredMethods_jdk6.put("public void java.lang.Runtime.traceInstructions(boolean)", "traceInstructions");
        testedDeclaredMethods_jdk6.put("public void java.lang.Runtime.traceMethodCalls(boolean)", "traceMethodCalls");
        testedDeclaredMethods_jdk6.put("public java.io.InputStream java.lang.Runtime.getLocalizedInputStream(java.io.InputStream)", "getLocalizedInputStream");
        testedDeclaredMethods_jdk6.put("public java.io.OutputStream java.lang.Runtime.getLocalizedOutputStream(java.io.OutputStream)", "getLocalizedOutputStream");

        // map for methods declared in (Open)JDK7
        testedDeclaredMethods_jdk7.put("public void java.lang.Runtime.exit(int)", "exit");
        testedDeclaredMethods_jdk7.put("public void java.lang.Runtime.runFinalization()", "runFinalization");
        testedDeclaredMethods_jdk7.put("public static void java.lang.Runtime.runFinalizersOnExit(boolean)", "runFinalizersOnExit");
        testedDeclaredMethods_jdk7.put("public void java.lang.Runtime.load(java.lang.String)", "load");
        testedDeclaredMethods_jdk7.put("public void java.lang.Runtime.loadLibrary(java.lang.String)", "loadLibrary");
        testedDeclaredMethods_jdk7.put("synchronized void java.lang.Runtime.loadLibrary0(java.lang.Class,java.lang.String)", "loadLibrary0");
        testedDeclaredMethods_jdk7.put("public void java.lang.Runtime.gc()", "gc");
        testedDeclaredMethods_jdk7.put("public static java.lang.Runtime java.lang.Runtime.getRuntime()", "getRuntime");
        testedDeclaredMethods_jdk7.put("synchronized void java.lang.Runtime.load0(java.lang.Class,java.lang.String)", "load0");
        testedDeclaredMethods_jdk7.put("public long java.lang.Runtime.freeMemory()", "freeMemory");
        testedDeclaredMethods_jdk7.put("public java.lang.Process java.lang.Runtime.exec(java.lang.String[]) throws java.io.IOException", "exec");
        testedDeclaredMethods_jdk7.put("public java.lang.Process java.lang.Runtime.exec(java.lang.String) throws java.io.IOException", "exec");
        testedDeclaredMethods_jdk7.put("public java.lang.Process java.lang.Runtime.exec(java.lang.String,java.lang.String[]) throws java.io.IOException", "exec");
        testedDeclaredMethods_jdk7.put("public java.lang.Process java.lang.Runtime.exec(java.lang.String[],java.lang.String[],java.io.File) throws java.io.IOException", "exec");
        testedDeclaredMethods_jdk7.put("public java.lang.Process java.lang.Runtime.exec(java.lang.String,java.lang.String[],java.io.File) throws java.io.IOException", "exec");
        testedDeclaredMethods_jdk7.put("public java.lang.Process java.lang.Runtime.exec(java.lang.String[],java.lang.String[]) throws java.io.IOException", "exec");
        testedDeclaredMethods_jdk7.put("public long java.lang.Runtime.maxMemory()", "maxMemory");
        testedDeclaredMethods_jdk7.put("public void java.lang.Runtime.addShutdownHook(java.lang.Thread)", "addShutdownHook");
        testedDeclaredMethods_jdk7.put("public int java.lang.Runtime.availableProcessors()", "availableProcessors");
        testedDeclaredMethods_jdk7.put("public java.io.InputStream java.lang.Runtime.getLocalizedInputStream(java.io.InputStream)", "getLocalizedInputStream");
        testedDeclaredMethods_jdk7.put("public java.io.OutputStream java.lang.Runtime.getLocalizedOutputStream(java.io.OutputStream)", "getLocalizedOutputStream");
        testedDeclaredMethods_jdk7.put("public void java.lang.Runtime.halt(int)", "halt");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Runtime.removeShutdownHook(java.lang.Thread)", "removeShutdownHook");
        testedDeclaredMethods_jdk7.put("private static void java.lang.Runtime.runFinalization0()", "runFinalization0");
        testedDeclaredMethods_jdk7.put("public long java.lang.Runtime.totalMemory()", "totalMemory");
        testedDeclaredMethods_jdk7.put("public void java.lang.Runtime.traceInstructions(boolean)", "traceInstructions");
        testedDeclaredMethods_jdk7.put("public void java.lang.Runtime.traceMethodCalls(boolean)", "traceMethodCalls");

        // create instance of a class Runtime
        final Object o = Runtime.getRuntime();

        // get a runtime class of an object "o"
        final Class c = o.getClass();

        // get the right map containing method signatures
        testedDeclaredMethods = getJavaVersion() < 7 ? testedDeclaredMethods_jdk6 : testedDeclaredMethods_jdk7;

        // get all declared methods for this class
        java.lang.reflect.Method[] declaredMethods = c.getDeclaredMethods();

        // expected number of declared methods
        final int expectedNumberOfDeclaredMethods = testedDeclaredMethods.size();

        // basic check for a number of declared methods
        harness.check(declaredMethods.length, expectedNumberOfDeclaredMethods);

        // check if all declared methods exist
        for (java.lang.reflect.Method declaredMethod : declaredMethods) {
            // method name should consists of package name + class name
            String methodName = declaredMethod.getName();
            // modifiers + package + method name + parameter types
            String methodString = declaredMethod.toString().replaceAll(" native ", " ");
            harness.check(testedDeclaredMethods.containsKey(methodString));
            harness.check(testedDeclaredMethods.get(methodString), methodName);
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

