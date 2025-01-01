// Test for method java.lang.Byte.getClass().getMethods()

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

package gnu.testlet.java.lang.Byte.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.Byte;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.lang.Byte.getClass().getMethods()
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
        testedMethods_jdk6.put("public boolean java.lang.Byte.equals(java.lang.Object)", "equals");
        testedMethods_jdk6.put("public java.lang.String java.lang.Byte.toString()", "toString");
        testedMethods_jdk6.put("public static java.lang.String java.lang.Byte.toString(byte)", "toString");
        testedMethods_jdk6.put("public int java.lang.Byte.hashCode()", "hashCode");
        testedMethods_jdk6.put("public int java.lang.Byte.compareTo(java.lang.Byte)", "compareTo");
        testedMethods_jdk6.put("public int java.lang.Byte.compareTo(java.lang.Object)", "compareTo");
        testedMethods_jdk6.put("public byte java.lang.Byte.byteValue()", "byteValue");
        testedMethods_jdk6.put("public short java.lang.Byte.shortValue()", "shortValue");
        testedMethods_jdk6.put("public int java.lang.Byte.intValue()", "intValue");
        testedMethods_jdk6.put("public long java.lang.Byte.longValue()", "longValue");
        testedMethods_jdk6.put("public float java.lang.Byte.floatValue()", "floatValue");
        testedMethods_jdk6.put("public double java.lang.Byte.doubleValue()", "doubleValue");
        testedMethods_jdk6.put("public static java.lang.Byte java.lang.Byte.valueOf(byte)", "valueOf");
        testedMethods_jdk6.put("public static java.lang.Byte java.lang.Byte.valueOf(java.lang.String,int) throws java.lang.NumberFormatException", "valueOf");
        testedMethods_jdk6.put("public static java.lang.Byte java.lang.Byte.valueOf(java.lang.String) throws java.lang.NumberFormatException", "valueOf");
        testedMethods_jdk6.put("public static java.lang.Byte java.lang.Byte.decode(java.lang.String) throws java.lang.NumberFormatException", "decode");
        testedMethods_jdk6.put("public static byte java.lang.Byte.parseByte(java.lang.String,int) throws java.lang.NumberFormatException", "parseByte");
        testedMethods_jdk6.put("public static byte java.lang.Byte.parseByte(java.lang.String) throws java.lang.NumberFormatException", "parseByte");
        testedMethods_jdk6.put("public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public final void java.lang.Object.wait(long) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public final void java.lang.Object.wait() throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public final java.lang.Class java.lang.Object.getClass()", "getClass");
        testedMethods_jdk6.put("public final void java.lang.Object.notify()", "notify");
        testedMethods_jdk6.put("public final void java.lang.Object.notifyAll()", "notifyAll");

        // map for methods declared in (Open)JDK7
        testedMethods_jdk7.put("public boolean java.lang.Byte.equals(java.lang.Object)", "equals");
        testedMethods_jdk7.put("public java.lang.String java.lang.Byte.toString()", "toString");
        testedMethods_jdk7.put("public static java.lang.String java.lang.Byte.toString(byte)", "toString");
        testedMethods_jdk7.put("public int java.lang.Byte.hashCode()", "hashCode");
        testedMethods_jdk7.put("public int java.lang.Byte.compareTo(java.lang.Byte)", "compareTo");
        testedMethods_jdk7.put("public int java.lang.Byte.compareTo(java.lang.Object)", "compareTo");
        testedMethods_jdk7.put("public byte java.lang.Byte.byteValue()", "byteValue");
        testedMethods_jdk7.put("public short java.lang.Byte.shortValue()", "shortValue");
        testedMethods_jdk7.put("public int java.lang.Byte.intValue()", "intValue");
        testedMethods_jdk7.put("public long java.lang.Byte.longValue()", "longValue");
        testedMethods_jdk7.put("public float java.lang.Byte.floatValue()", "floatValue");
        testedMethods_jdk7.put("public double java.lang.Byte.doubleValue()", "doubleValue");
        testedMethods_jdk7.put("public static java.lang.Byte java.lang.Byte.valueOf(byte)", "valueOf");
        testedMethods_jdk7.put("public static java.lang.Byte java.lang.Byte.valueOf(java.lang.String) throws java.lang.NumberFormatException", "valueOf");
        testedMethods_jdk7.put("public static java.lang.Byte java.lang.Byte.valueOf(java.lang.String,int) throws java.lang.NumberFormatException", "valueOf");
        testedMethods_jdk7.put("public static int java.lang.Byte.compare(byte,byte)", "compare");
        testedMethods_jdk7.put("public static java.lang.Byte java.lang.Byte.decode(java.lang.String) throws java.lang.NumberFormatException", "decode");
        testedMethods_jdk7.put("public static byte java.lang.Byte.parseByte(java.lang.String) throws java.lang.NumberFormatException", "parseByte");
        testedMethods_jdk7.put("public static byte java.lang.Byte.parseByte(java.lang.String,int) throws java.lang.NumberFormatException", "parseByte");
        testedMethods_jdk7.put("public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public final void java.lang.Object.wait(long) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public final void java.lang.Object.wait() throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public final java.lang.Class java.lang.Object.getClass()", "getClass");
        testedMethods_jdk7.put("public final void java.lang.Object.notify()", "notify");
        testedMethods_jdk7.put("public final void java.lang.Object.notifyAll()", "notifyAll");

        // create instance of a class Byte
        final Object o = new Byte((byte)42);

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

