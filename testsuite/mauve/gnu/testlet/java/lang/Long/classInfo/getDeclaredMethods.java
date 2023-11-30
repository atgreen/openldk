// Test for method java.lang.Long.getClass().getDeclaredMethods()

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

package gnu.testlet.java.lang.Long.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.Long;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.lang.Long.getClass().getDeclaredMethods()
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
        testedDeclaredMethods_jdk6.put("public static int java.lang.Long.numberOfLeadingZeros(long)", "numberOfLeadingZeros");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Long.numberOfTrailingZeros(long)", "numberOfTrailingZeros");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Long.bitCount(long)", "bitCount");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Long.equals(java.lang.Object)", "equals");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.Long.toString()", "toString");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.Long.toString(long,int)", "toString");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.Long.toString(long)", "toString");
        testedDeclaredMethods_jdk6.put("public int java.lang.Long.hashCode()", "hashCode");
        testedDeclaredMethods_jdk6.put("public static long java.lang.Long.reverseBytes(long)", "reverseBytes");
        testedDeclaredMethods_jdk6.put("public int java.lang.Long.compareTo(java.lang.Long)", "compareTo");
        testedDeclaredMethods_jdk6.put("public int java.lang.Long.compareTo(java.lang.Object)", "compareTo");
        testedDeclaredMethods_jdk6.put("public static java.lang.Long java.lang.Long.getLong(java.lang.String)", "getLong");
        testedDeclaredMethods_jdk6.put("public static java.lang.Long java.lang.Long.getLong(java.lang.String,java.lang.Long)", "getLong");
        testedDeclaredMethods_jdk6.put("public static java.lang.Long java.lang.Long.getLong(java.lang.String,long)", "getLong");
        testedDeclaredMethods_jdk6.put("public byte java.lang.Long.byteValue()", "byteValue");
        testedDeclaredMethods_jdk6.put("public short java.lang.Long.shortValue()", "shortValue");
        testedDeclaredMethods_jdk6.put("public int java.lang.Long.intValue()", "intValue");
        testedDeclaredMethods_jdk6.put("public long java.lang.Long.longValue()", "longValue");
        testedDeclaredMethods_jdk6.put("public float java.lang.Long.floatValue()", "floatValue");
        testedDeclaredMethods_jdk6.put("public double java.lang.Long.doubleValue()", "doubleValue");
        testedDeclaredMethods_jdk6.put("public static java.lang.Long java.lang.Long.valueOf(java.lang.String) throws java.lang.NumberFormatException", "valueOf");
        testedDeclaredMethods_jdk6.put("public static java.lang.Long java.lang.Long.valueOf(long)", "valueOf");
        testedDeclaredMethods_jdk6.put("public static java.lang.Long java.lang.Long.valueOf(java.lang.String,int) throws java.lang.NumberFormatException", "valueOf");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.Long.toHexString(long)", "toHexString");
        testedDeclaredMethods_jdk6.put("static void java.lang.Long.getChars(long,int,char[])", "getChars");
        testedDeclaredMethods_jdk6.put("public static java.lang.Long java.lang.Long.decode(java.lang.String) throws java.lang.NumberFormatException", "decode");
        testedDeclaredMethods_jdk6.put("public static long java.lang.Long.reverse(long)", "reverse");
        testedDeclaredMethods_jdk6.put("static int java.lang.Long.stringSize(long)", "stringSize");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.Long.toOctalString(long)", "toOctalString");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.Long.toBinaryString(long)", "toBinaryString");
        testedDeclaredMethods_jdk6.put("private static java.lang.String java.lang.Long.toUnsignedString(long,int)", "toUnsignedString");
        testedDeclaredMethods_jdk6.put("public static long java.lang.Long.highestOneBit(long)", "highestOneBit");
        testedDeclaredMethods_jdk6.put("public static long java.lang.Long.lowestOneBit(long)", "lowestOneBit");
        testedDeclaredMethods_jdk6.put("public static long java.lang.Long.rotateLeft(long,int)", "rotateLeft");
        testedDeclaredMethods_jdk6.put("public static long java.lang.Long.rotateRight(long,int)", "rotateRight");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Long.signum(long)", "signum");
        testedDeclaredMethods_jdk6.put("public static long java.lang.Long.parseLong(java.lang.String) throws java.lang.NumberFormatException", "parseLong");
        testedDeclaredMethods_jdk6.put("public static long java.lang.Long.parseLong(java.lang.String,int) throws java.lang.NumberFormatException", "parseLong");

        // map for methods declared in (Open)JDK7
        testedDeclaredMethods_jdk7.put("public static int java.lang.Long.numberOfLeadingZeros(long)", "numberOfLeadingZeros");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Long.numberOfTrailingZeros(long)", "numberOfTrailingZeros");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Long.bitCount(long)", "bitCount");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Long.equals(java.lang.Object)", "equals");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.Long.toString()", "toString");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.Long.toString(long,int)", "toString");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.Long.toString(long)", "toString");
        testedDeclaredMethods_jdk7.put("public int java.lang.Long.hashCode()", "hashCode");
        testedDeclaredMethods_jdk7.put("public static long java.lang.Long.reverseBytes(long)", "reverseBytes");
        testedDeclaredMethods_jdk7.put("public int java.lang.Long.compareTo(java.lang.Long)", "compareTo");
        testedDeclaredMethods_jdk7.put("public int java.lang.Long.compareTo(java.lang.Object)", "compareTo");
        testedDeclaredMethods_jdk7.put("public static java.lang.Long java.lang.Long.getLong(java.lang.String)", "getLong");
        testedDeclaredMethods_jdk7.put("public static java.lang.Long java.lang.Long.getLong(java.lang.String,java.lang.Long)", "getLong");
        testedDeclaredMethods_jdk7.put("public static java.lang.Long java.lang.Long.getLong(java.lang.String,long)", "getLong");
        testedDeclaredMethods_jdk7.put("public byte java.lang.Long.byteValue()", "byteValue");
        testedDeclaredMethods_jdk7.put("public short java.lang.Long.shortValue()", "shortValue");
        testedDeclaredMethods_jdk7.put("public int java.lang.Long.intValue()", "intValue");
        testedDeclaredMethods_jdk7.put("public long java.lang.Long.longValue()", "longValue");
        testedDeclaredMethods_jdk7.put("public float java.lang.Long.floatValue()", "floatValue");
        testedDeclaredMethods_jdk7.put("public double java.lang.Long.doubleValue()", "doubleValue");
        testedDeclaredMethods_jdk7.put("public static java.lang.Long java.lang.Long.valueOf(long)", "valueOf");
        testedDeclaredMethods_jdk7.put("public static java.lang.Long java.lang.Long.valueOf(java.lang.String,int) throws java.lang.NumberFormatException", "valueOf");
        testedDeclaredMethods_jdk7.put("public static java.lang.Long java.lang.Long.valueOf(java.lang.String) throws java.lang.NumberFormatException", "valueOf");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.Long.toHexString(long)", "toHexString");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Long.compare(long,long)", "compare");
        testedDeclaredMethods_jdk7.put("public static java.lang.Long java.lang.Long.decode(java.lang.String) throws java.lang.NumberFormatException", "decode");
        testedDeclaredMethods_jdk7.put("static void java.lang.Long.getChars(long,int,char[])", "getChars");
        testedDeclaredMethods_jdk7.put("public static long java.lang.Long.reverse(long)", "reverse");
        testedDeclaredMethods_jdk7.put("static int java.lang.Long.stringSize(long)", "stringSize");
        testedDeclaredMethods_jdk7.put("public static long java.lang.Long.highestOneBit(long)", "highestOneBit");
        testedDeclaredMethods_jdk7.put("public static long java.lang.Long.lowestOneBit(long)", "lowestOneBit");
        testedDeclaredMethods_jdk7.put("public static long java.lang.Long.rotateLeft(long,int)", "rotateLeft");
        testedDeclaredMethods_jdk7.put("public static long java.lang.Long.rotateRight(long,int)", "rotateRight");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Long.signum(long)", "signum");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.Long.toBinaryString(long)", "toBinaryString");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.Long.toOctalString(long)", "toOctalString");
        testedDeclaredMethods_jdk7.put("private static java.lang.String java.lang.Long.toUnsignedString(long,int)", "toUnsignedString");
        testedDeclaredMethods_jdk7.put("public static long java.lang.Long.parseLong(java.lang.String) throws java.lang.NumberFormatException", "parseLong");
        testedDeclaredMethods_jdk7.put("public static long java.lang.Long.parseLong(java.lang.String,int) throws java.lang.NumberFormatException", "parseLong");

        // create instance of a class Long
        final Object o = new Long(42);

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

