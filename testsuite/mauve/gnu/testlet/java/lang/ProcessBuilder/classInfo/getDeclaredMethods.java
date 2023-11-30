// Test for method java.lang.ProcessBuilder.getClass().getDeclaredMethods()

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

package gnu.testlet.java.lang.ProcessBuilder.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.ProcessBuilder;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.lang.ProcessBuilder.getClass().getDeclaredMethods()
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
        testedDeclaredMethods_jdk6.put("public java.lang.Process java.lang.ProcessBuilder.start() throws java.io.IOException", "start");
        testedDeclaredMethods_jdk6.put("public java.lang.ProcessBuilder java.lang.ProcessBuilder.command(java.util.List)", "command");
        testedDeclaredMethods_jdk6.put("public java.lang.ProcessBuilder java.lang.ProcessBuilder.command(java.lang.String[])", "command");
        testedDeclaredMethods_jdk6.put("public java.util.List java.lang.ProcessBuilder.command()", "command");
        testedDeclaredMethods_jdk6.put("java.lang.ProcessBuilder java.lang.ProcessBuilder.environment(java.lang.String[])", "environment");
        testedDeclaredMethods_jdk6.put("public java.util.Map java.lang.ProcessBuilder.environment()", "environment");
        testedDeclaredMethods_jdk6.put("public java.lang.ProcessBuilder java.lang.ProcessBuilder.directory(java.io.File)", "directory");
        testedDeclaredMethods_jdk6.put("public java.io.File java.lang.ProcessBuilder.directory()", "directory");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.ProcessBuilder.redirectErrorStream()", "redirectErrorStream");
        testedDeclaredMethods_jdk6.put("public java.lang.ProcessBuilder java.lang.ProcessBuilder.redirectErrorStream(boolean)", "redirectErrorStream");

        // map for methods declared in (Open)JDK7
        testedDeclaredMethods_jdk7.put("public java.lang.Process java.lang.ProcessBuilder.start() throws java.io.IOException", "start");
        testedDeclaredMethods_jdk7.put("public java.io.File java.lang.ProcessBuilder.directory()", "directory");
        testedDeclaredMethods_jdk7.put("public java.lang.ProcessBuilder java.lang.ProcessBuilder.directory(java.io.File)", "directory");
        testedDeclaredMethods_jdk7.put("public java.util.Map java.lang.ProcessBuilder.environment()", "environment");
        testedDeclaredMethods_jdk7.put("java.lang.ProcessBuilder java.lang.ProcessBuilder.environment(java.lang.String[])", "environment");
        testedDeclaredMethods_jdk7.put("public java.lang.ProcessBuilder java.lang.ProcessBuilder.command(java.util.List)", "command");
        testedDeclaredMethods_jdk7.put("public java.util.List java.lang.ProcessBuilder.command()", "command");
        testedDeclaredMethods_jdk7.put("public java.lang.ProcessBuilder java.lang.ProcessBuilder.command(java.lang.String[])", "command");
        testedDeclaredMethods_jdk7.put("public java.lang.ProcessBuilder java.lang.ProcessBuilder.inheritIO()", "inheritIO");
        testedDeclaredMethods_jdk7.put("public java.lang.ProcessBuilder java.lang.ProcessBuilder.redirectError(java.io.File)", "redirectError");
        testedDeclaredMethods_jdk7.put("public java.lang.ProcessBuilder$Redirect java.lang.ProcessBuilder.redirectError()", "redirectError");
        testedDeclaredMethods_jdk7.put("public java.lang.ProcessBuilder java.lang.ProcessBuilder.redirectError(java.lang.ProcessBuilder$Redirect)", "redirectError");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.ProcessBuilder.redirectErrorStream()", "redirectErrorStream");
        testedDeclaredMethods_jdk7.put("public java.lang.ProcessBuilder java.lang.ProcessBuilder.redirectErrorStream(boolean)", "redirectErrorStream");
        testedDeclaredMethods_jdk7.put("public java.lang.ProcessBuilder java.lang.ProcessBuilder.redirectInput(java.io.File)", "redirectInput");
        testedDeclaredMethods_jdk7.put("public java.lang.ProcessBuilder$Redirect java.lang.ProcessBuilder.redirectInput()", "redirectInput");
        testedDeclaredMethods_jdk7.put("public java.lang.ProcessBuilder java.lang.ProcessBuilder.redirectInput(java.lang.ProcessBuilder$Redirect)", "redirectInput");
        testedDeclaredMethods_jdk7.put("public java.lang.ProcessBuilder java.lang.ProcessBuilder.redirectOutput(java.io.File)", "redirectOutput");
        testedDeclaredMethods_jdk7.put("public java.lang.ProcessBuilder java.lang.ProcessBuilder.redirectOutput(java.lang.ProcessBuilder$Redirect)", "redirectOutput");
        testedDeclaredMethods_jdk7.put("public java.lang.ProcessBuilder$Redirect java.lang.ProcessBuilder.redirectOutput()", "redirectOutput");
        testedDeclaredMethods_jdk7.put("private java.lang.ProcessBuilder$Redirect[] java.lang.ProcessBuilder.redirects()", "redirects");

        // create instance of a class ProcessBuilder
        final Object o = new ProcessBuilder("");

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

