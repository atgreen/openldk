// Test for method java.lang.Integer.getClass().getMethods()

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

package gnu.testlet.java.lang.Integer.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.Integer;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.lang.Integer.getClass().getMethods()
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
        testedMethods_jdk6.put("public static int java.lang.Integer.numberOfLeadingZeros(int)", "numberOfLeadingZeros");
        testedMethods_jdk6.put("public static int java.lang.Integer.numberOfTrailingZeros(int)", "numberOfTrailingZeros");
        testedMethods_jdk6.put("public static int java.lang.Integer.bitCount(int)", "bitCount");
        testedMethods_jdk6.put("public boolean java.lang.Integer.equals(java.lang.Object)", "equals");
        testedMethods_jdk6.put("public java.lang.String java.lang.Integer.toString()", "toString");
        testedMethods_jdk6.put("public static java.lang.String java.lang.Integer.toString(int,int)", "toString");
        testedMethods_jdk6.put("public static java.lang.String java.lang.Integer.toString(int)", "toString");
        testedMethods_jdk6.put("public int java.lang.Integer.hashCode()", "hashCode");
        testedMethods_jdk6.put("public static int java.lang.Integer.reverseBytes(int)", "reverseBytes");
        testedMethods_jdk6.put("public int java.lang.Integer.compareTo(java.lang.Object)", "compareTo");
        testedMethods_jdk6.put("public int java.lang.Integer.compareTo(java.lang.Integer)", "compareTo");
        testedMethods_jdk6.put("public byte java.lang.Integer.byteValue()", "byteValue");
        testedMethods_jdk6.put("public short java.lang.Integer.shortValue()", "shortValue");
        testedMethods_jdk6.put("public int java.lang.Integer.intValue()", "intValue");
        testedMethods_jdk6.put("public long java.lang.Integer.longValue()", "longValue");
        testedMethods_jdk6.put("public float java.lang.Integer.floatValue()", "floatValue");
        testedMethods_jdk6.put("public double java.lang.Integer.doubleValue()", "doubleValue");
        testedMethods_jdk6.put("public static java.lang.Integer java.lang.Integer.valueOf(int)", "valueOf");
        testedMethods_jdk6.put("public static java.lang.Integer java.lang.Integer.valueOf(java.lang.String) throws java.lang.NumberFormatException", "valueOf");
        testedMethods_jdk6.put("public static java.lang.Integer java.lang.Integer.valueOf(java.lang.String,int) throws java.lang.NumberFormatException", "valueOf");
        testedMethods_jdk6.put("public static java.lang.String java.lang.Integer.toHexString(int)", "toHexString");
        testedMethods_jdk6.put("public static java.lang.Integer java.lang.Integer.decode(java.lang.String) throws java.lang.NumberFormatException", "decode");
        testedMethods_jdk6.put("public static int java.lang.Integer.reverse(int)", "reverse");
        testedMethods_jdk6.put("public static int java.lang.Integer.parseInt(java.lang.String,int) throws java.lang.NumberFormatException", "parseInt");
        testedMethods_jdk6.put("public static int java.lang.Integer.parseInt(java.lang.String) throws java.lang.NumberFormatException", "parseInt");
        testedMethods_jdk6.put("public static java.lang.String java.lang.Integer.toOctalString(int)", "toOctalString");
        testedMethods_jdk6.put("public static java.lang.String java.lang.Integer.toBinaryString(int)", "toBinaryString");
        testedMethods_jdk6.put("public static java.lang.Integer java.lang.Integer.getInteger(java.lang.String)", "getInteger");
        testedMethods_jdk6.put("public static java.lang.Integer java.lang.Integer.getInteger(java.lang.String,int)", "getInteger");
        testedMethods_jdk6.put("public static java.lang.Integer java.lang.Integer.getInteger(java.lang.String,java.lang.Integer)", "getInteger");
        testedMethods_jdk6.put("public static int java.lang.Integer.highestOneBit(int)", "highestOneBit");
        testedMethods_jdk6.put("public static int java.lang.Integer.lowestOneBit(int)", "lowestOneBit");
        testedMethods_jdk6.put("public static int java.lang.Integer.rotateLeft(int,int)", "rotateLeft");
        testedMethods_jdk6.put("public static int java.lang.Integer.rotateRight(int,int)", "rotateRight");
        testedMethods_jdk6.put("public static int java.lang.Integer.signum(int)", "signum");
        testedMethods_jdk6.put("public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public final void java.lang.Object.wait(long) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public final void java.lang.Object.wait() throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public final java.lang.Class java.lang.Object.getClass()", "getClass");
        testedMethods_jdk6.put("public final void java.lang.Object.notify()", "notify");
        testedMethods_jdk6.put("public final void java.lang.Object.notifyAll()", "notifyAll");

        // map for methods declared in (Open)JDK7
        testedMethods_jdk7.put("public static int java.lang.Integer.numberOfLeadingZeros(int)", "numberOfLeadingZeros");
        testedMethods_jdk7.put("public static int java.lang.Integer.numberOfTrailingZeros(int)", "numberOfTrailingZeros");
        testedMethods_jdk7.put("public static int java.lang.Integer.bitCount(int)", "bitCount");
        testedMethods_jdk7.put("public boolean java.lang.Integer.equals(java.lang.Object)", "equals");
        testedMethods_jdk7.put("public java.lang.String java.lang.Integer.toString()", "toString");
        testedMethods_jdk7.put("public static java.lang.String java.lang.Integer.toString(int,int)", "toString");
        testedMethods_jdk7.put("public static java.lang.String java.lang.Integer.toString(int)", "toString");
        testedMethods_jdk7.put("public int java.lang.Integer.hashCode()", "hashCode");
        testedMethods_jdk7.put("public static int java.lang.Integer.reverseBytes(int)", "reverseBytes");
        testedMethods_jdk7.put("public int java.lang.Integer.compareTo(java.lang.Object)", "compareTo");
        testedMethods_jdk7.put("public int java.lang.Integer.compareTo(java.lang.Integer)", "compareTo");
        testedMethods_jdk7.put("public byte java.lang.Integer.byteValue()", "byteValue");
        testedMethods_jdk7.put("public short java.lang.Integer.shortValue()", "shortValue");
        testedMethods_jdk7.put("public int java.lang.Integer.intValue()", "intValue");
        testedMethods_jdk7.put("public long java.lang.Integer.longValue()", "longValue");
        testedMethods_jdk7.put("public float java.lang.Integer.floatValue()", "floatValue");
        testedMethods_jdk7.put("public double java.lang.Integer.doubleValue()", "doubleValue");
        testedMethods_jdk7.put("public static java.lang.Integer java.lang.Integer.valueOf(int)", "valueOf");
        testedMethods_jdk7.put("public static java.lang.Integer java.lang.Integer.valueOf(java.lang.String) throws java.lang.NumberFormatException", "valueOf");
        testedMethods_jdk7.put("public static java.lang.Integer java.lang.Integer.valueOf(java.lang.String,int) throws java.lang.NumberFormatException", "valueOf");
        testedMethods_jdk7.put("public static java.lang.String java.lang.Integer.toHexString(int)", "toHexString");
        testedMethods_jdk7.put("public static int java.lang.Integer.compare(int,int)", "compare");
        testedMethods_jdk7.put("public static java.lang.Integer java.lang.Integer.decode(java.lang.String) throws java.lang.NumberFormatException", "decode");
        testedMethods_jdk7.put("public static int java.lang.Integer.reverse(int)", "reverse");
        testedMethods_jdk7.put("public static int java.lang.Integer.parseInt(java.lang.String) throws java.lang.NumberFormatException", "parseInt");
        testedMethods_jdk7.put("public static int java.lang.Integer.parseInt(java.lang.String,int) throws java.lang.NumberFormatException", "parseInt");
        testedMethods_jdk7.put("public static java.lang.Integer java.lang.Integer.getInteger(java.lang.String)", "getInteger");
        testedMethods_jdk7.put("public static java.lang.Integer java.lang.Integer.getInteger(java.lang.String,int)", "getInteger");
        testedMethods_jdk7.put("public static java.lang.Integer java.lang.Integer.getInteger(java.lang.String,java.lang.Integer)", "getInteger");
        testedMethods_jdk7.put("public static int java.lang.Integer.highestOneBit(int)", "highestOneBit");
        testedMethods_jdk7.put("public static int java.lang.Integer.lowestOneBit(int)", "lowestOneBit");
        testedMethods_jdk7.put("public static int java.lang.Integer.rotateLeft(int,int)", "rotateLeft");
        testedMethods_jdk7.put("public static int java.lang.Integer.rotateRight(int,int)", "rotateRight");
        testedMethods_jdk7.put("public static int java.lang.Integer.signum(int)", "signum");
        testedMethods_jdk7.put("public static java.lang.String java.lang.Integer.toBinaryString(int)", "toBinaryString");
        testedMethods_jdk7.put("public static java.lang.String java.lang.Integer.toOctalString(int)", "toOctalString");
        testedMethods_jdk7.put("public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public final void java.lang.Object.wait(long) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public final void java.lang.Object.wait() throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public final java.lang.Class java.lang.Object.getClass()", "getClass");
        testedMethods_jdk7.put("public final void java.lang.Object.notify()", "notify");
        testedMethods_jdk7.put("public final void java.lang.Object.notifyAll()", "notifyAll");

        // create instance of a class Integer
        final Object o = new Integer(42);

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

