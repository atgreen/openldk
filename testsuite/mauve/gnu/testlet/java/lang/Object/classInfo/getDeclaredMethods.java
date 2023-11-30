// Test for method java.lang.Object.getClass().getDeclaredMethods()

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

package gnu.testlet.java.lang.Object.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.Object;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.lang.Object.getClass().getDeclaredMethods()
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
        testedDeclaredMethods_jdk6.put("protected void java.lang.Object.finalize() throws java.lang.Throwable", "finalize");
        testedDeclaredMethods_jdk6.put("public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException", "wait");
        testedDeclaredMethods_jdk6.put("public final void java.lang.Object.wait(long) throws java.lang.InterruptedException", "wait");
        testedDeclaredMethods_jdk6.put("public final void java.lang.Object.wait() throws java.lang.InterruptedException", "wait");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Object.equals(java.lang.Object)", "equals");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.Object.toString()", "toString");
        testedDeclaredMethods_jdk6.put("public int java.lang.Object.hashCode()", "hashCode");
        testedDeclaredMethods_jdk6.put("public final java.lang.Class java.lang.Object.getClass()", "getClass");
        testedDeclaredMethods_jdk6.put("protected java.lang.Object java.lang.Object.clone() throws java.lang.CloneNotSupportedException", "clone");
        testedDeclaredMethods_jdk6.put("private static void java.lang.Object.registerNatives()", "registerNatives");
        testedDeclaredMethods_jdk6.put("public final void java.lang.Object.notify()", "notify");
        testedDeclaredMethods_jdk6.put("public final void java.lang.Object.notifyAll()", "notifyAll");

        // map for methods declared in (Open)JDK7
        testedDeclaredMethods_jdk7.put("protected void java.lang.Object.finalize() throws java.lang.Throwable", "finalize");
        testedDeclaredMethods_jdk7.put("public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException", "wait");
        testedDeclaredMethods_jdk7.put("public final void java.lang.Object.wait(long) throws java.lang.InterruptedException", "wait");
        testedDeclaredMethods_jdk7.put("public final void java.lang.Object.wait() throws java.lang.InterruptedException", "wait");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Object.equals(java.lang.Object)", "equals");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.Object.toString()", "toString");
        testedDeclaredMethods_jdk7.put("public int java.lang.Object.hashCode()", "hashCode");
        testedDeclaredMethods_jdk7.put("public final java.lang.Class java.lang.Object.getClass()", "getClass");
        testedDeclaredMethods_jdk7.put("protected java.lang.Object java.lang.Object.clone() throws java.lang.CloneNotSupportedException", "clone");
        testedDeclaredMethods_jdk7.put("private static void java.lang.Object.registerNatives()", "registerNatives");
        testedDeclaredMethods_jdk7.put("public final void java.lang.Object.notify()", "notify");
        testedDeclaredMethods_jdk7.put("public final void java.lang.Object.notifyAll()", "notifyAll");

        // create instance of a class Object
        final Object o = new Object();

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

