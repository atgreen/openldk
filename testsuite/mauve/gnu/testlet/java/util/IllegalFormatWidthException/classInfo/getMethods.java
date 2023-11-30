// Test for method java.util.IllegalFormatWidthException.getClass().getMethods()

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

package gnu.testlet.java.util.IllegalFormatWidthException.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.IllegalFormatWidthException;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.util.IllegalFormatWidthException.getClass().getMethods()
 */
public class getMethods implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // map of methods which should exists
        Map<String, String> testedMethods = null;

        // map of methods for (Open)JDK6
        Map<String, String> testedMethods_jdk6 = new HashMap<String, String>();

        // map of methods for (Open)JDK7
        Map<String, String> testedMethods_jdk7 = new HashMap<String, String>();

        // map for methods declared in (Open)JDK6
        testedMethods_jdk6.put("public java.lang.String java.util.IllegalFormatWidthException.getMessage()", "getMessage");
        testedMethods_jdk6.put("public int java.util.IllegalFormatWidthException.getWidth()", "getWidth");
        testedMethods_jdk6.put("public void java.lang.Throwable.printStackTrace()", "printStackTrace");
        testedMethods_jdk6.put("public void java.lang.Throwable.printStackTrace(java.io.PrintStream)", "printStackTrace");
        testedMethods_jdk6.put("public void java.lang.Throwable.printStackTrace(java.io.PrintWriter)", "printStackTrace");
        testedMethods_jdk6.put("public synchronized java.lang.Throwable java.lang.Throwable.fillInStackTrace()", "fillInStackTrace");
        testedMethods_jdk6.put("public java.lang.Throwable java.lang.Throwable.getCause()", "getCause");
        testedMethods_jdk6.put("public synchronized java.lang.Throwable java.lang.Throwable.initCause(java.lang.Throwable)", "initCause");
        testedMethods_jdk6.put("public java.lang.String java.lang.Throwable.toString()", "toString");
        testedMethods_jdk6.put("public java.lang.String java.lang.Throwable.getLocalizedMessage()", "getLocalizedMessage");
        testedMethods_jdk6.put("public java.lang.StackTraceElement[] java.lang.Throwable.getStackTrace()", "getStackTrace");
        testedMethods_jdk6.put("public void java.lang.Throwable.setStackTrace(java.lang.StackTraceElement[])", "setStackTrace");
        testedMethods_jdk6.put("public final void java.lang.Object.wait(long) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public final void java.lang.Object.wait() throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public int java.lang.Object.hashCode()", "hashCode");
        testedMethods_jdk6.put("public final java.lang.Class java.lang.Object.getClass()", "getClass");
        testedMethods_jdk6.put("public boolean java.lang.Object.equals(java.lang.Object)", "equals");
        testedMethods_jdk6.put("public final void java.lang.Object.notify()", "notify");
        testedMethods_jdk6.put("public final void java.lang.Object.notifyAll()", "notifyAll");

        // map for methods declared in (Open)JDK7
        testedMethods_jdk7.put("public java.lang.String java.util.IllegalFormatWidthException.getMessage()", "getMessage");
        testedMethods_jdk7.put("public int java.util.IllegalFormatWidthException.getWidth()", "getWidth");
        testedMethods_jdk7.put("public void java.lang.Throwable.printStackTrace()", "printStackTrace");
        testedMethods_jdk7.put("public void java.lang.Throwable.printStackTrace(java.io.PrintWriter)", "printStackTrace");
        testedMethods_jdk7.put("public void java.lang.Throwable.printStackTrace(java.io.PrintStream)", "printStackTrace");
        testedMethods_jdk7.put("public synchronized java.lang.Throwable java.lang.Throwable.fillInStackTrace()", "fillInStackTrace");
        testedMethods_jdk7.put("public synchronized java.lang.Throwable java.lang.Throwable.getCause()", "getCause");
        testedMethods_jdk7.put("public synchronized java.lang.Throwable java.lang.Throwable.initCause(java.lang.Throwable)", "initCause");
        testedMethods_jdk7.put("public java.lang.String java.lang.Throwable.toString()", "toString");
        testedMethods_jdk7.put("public java.lang.String java.lang.Throwable.getLocalizedMessage()", "getLocalizedMessage");
        testedMethods_jdk7.put("public java.lang.StackTraceElement[] java.lang.Throwable.getStackTrace()", "getStackTrace");
        testedMethods_jdk7.put("public void java.lang.Throwable.setStackTrace(java.lang.StackTraceElement[])", "setStackTrace");
        testedMethods_jdk7.put("public final synchronized void java.lang.Throwable.addSuppressed(java.lang.Throwable)", "addSuppressed");
        testedMethods_jdk7.put("public final synchronized java.lang.Throwable[] java.lang.Throwable.getSuppressed()", "getSuppressed");
        testedMethods_jdk7.put("public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public final void java.lang.Object.wait(long) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public final void java.lang.Object.wait() throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public boolean java.lang.Object.equals(java.lang.Object)", "equals");
        testedMethods_jdk7.put("public int java.lang.Object.hashCode()", "hashCode");
        testedMethods_jdk7.put("public final java.lang.Class java.lang.Object.getClass()", "getClass");
        testedMethods_jdk7.put("public final void java.lang.Object.notify()", "notify");
        testedMethods_jdk7.put("public final void java.lang.Object.notifyAll()", "notifyAll");

        // create instance of a class IllegalFormatWidthException
        final Object o = new IllegalFormatWidthException(42);

        // get a runtime class of an object "o"
        final Class c = o.getClass();

        // get the right map containing method signatures
        testedMethods = getJavaVersion() < 7 ? testedMethods_jdk6 : testedMethods_jdk7;

        // get all methods for this class
        java.lang.reflect.Method[] methods = c.getMethods();

        // expected number of methods
        final int expectedNumberOfMethods = testedMethods.size();

        // basic check for a number of methods
        harness.check(methods.length, expectedNumberOfMethods);

        // check if all methods exist
        for (java.lang.reflect.Method method : methods) {
            // method name should consists of package name + class name
            String methodName = method.getName();
            // modifiers + package + method name + parameter types
            String methodString = method.toString().replaceAll(" native ", " ");
            harness.check(testedMethods.containsKey(methodString));
            harness.check(testedMethods.get(methodString), methodName);
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

