// Test for method java.lang.String.getClass().getDeclaredMethods()

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

package gnu.testlet.java.lang.String.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.String;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.lang.String.getClass().getDeclaredMethods()
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
        testedDeclaredMethods_jdk6.put("public boolean java.lang.String.equals(java.lang.Object)", "equals");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.String.toString()", "toString");
        testedDeclaredMethods_jdk6.put("public int java.lang.String.hashCode()", "hashCode");
        testedDeclaredMethods_jdk6.put("public int java.lang.String.compareTo(java.lang.String)", "compareTo");
        testedDeclaredMethods_jdk6.put("public int java.lang.String.compareTo(java.lang.Object)", "compareTo");
        testedDeclaredMethods_jdk6.put("public int java.lang.String.indexOf(int,int)", "indexOf");
        testedDeclaredMethods_jdk6.put("static int java.lang.String.indexOf(char[],int,int,char[],int,int,int)", "indexOf");
        testedDeclaredMethods_jdk6.put("public int java.lang.String.indexOf(java.lang.String,int)", "indexOf");
        testedDeclaredMethods_jdk6.put("public int java.lang.String.indexOf(java.lang.String)", "indexOf");
        testedDeclaredMethods_jdk6.put("public int java.lang.String.indexOf(int)", "indexOf");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(int)", "valueOf");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(long)", "valueOf");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(float)", "valueOf");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(double)", "valueOf");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(java.lang.Object)", "valueOf");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(char[],int,int)", "valueOf");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(char[])", "valueOf");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(char)", "valueOf");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(boolean)", "valueOf");
        testedDeclaredMethods_jdk6.put("private static void java.lang.String.checkBounds(byte[],int,int)", "checkBounds");
        testedDeclaredMethods_jdk6.put("public int java.lang.String.length()", "length");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.String.isEmpty()", "isEmpty");
        testedDeclaredMethods_jdk6.put("public char java.lang.String.charAt(int)", "charAt");
        testedDeclaredMethods_jdk6.put("public int java.lang.String.codePointAt(int)", "codePointAt");
        testedDeclaredMethods_jdk6.put("public int java.lang.String.codePointBefore(int)", "codePointBefore");
        testedDeclaredMethods_jdk6.put("public int java.lang.String.codePointCount(int,int)", "codePointCount");
        testedDeclaredMethods_jdk6.put("public int java.lang.String.offsetByCodePoints(int,int)", "offsetByCodePoints");
        testedDeclaredMethods_jdk6.put("void java.lang.String.getChars(char[],int)", "getChars");
        testedDeclaredMethods_jdk6.put("public void java.lang.String.getChars(int,int,char[],int)", "getChars");
        testedDeclaredMethods_jdk6.put("public byte[] java.lang.String.getBytes(java.nio.charset.Charset)", "getBytes");
        testedDeclaredMethods_jdk6.put("public byte[] java.lang.String.getBytes()", "getBytes");
        testedDeclaredMethods_jdk6.put("public byte[] java.lang.String.getBytes(java.lang.String) throws java.io.UnsupportedEncodingException", "getBytes");
        testedDeclaredMethods_jdk6.put("public void java.lang.String.getBytes(int,int,byte[],int)", "getBytes");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.String.contentEquals(java.lang.CharSequence)", "contentEquals");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.String.contentEquals(java.lang.StringBuffer)", "contentEquals");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.String.equalsIgnoreCase(java.lang.String)", "equalsIgnoreCase");
        testedDeclaredMethods_jdk6.put("public int java.lang.String.compareToIgnoreCase(java.lang.String)", "compareToIgnoreCase");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.String.regionMatches(boolean,int,java.lang.String,int,int)", "regionMatches");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.String.regionMatches(int,java.lang.String,int,int)", "regionMatches");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.String.startsWith(java.lang.String)", "startsWith");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.String.startsWith(java.lang.String,int)", "startsWith");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.String.endsWith(java.lang.String)", "endsWith");
        testedDeclaredMethods_jdk6.put("public int java.lang.String.lastIndexOf(int)", "lastIndexOf");
        testedDeclaredMethods_jdk6.put("static int java.lang.String.lastIndexOf(char[],int,int,char[],int,int,int)", "lastIndexOf");
        testedDeclaredMethods_jdk6.put("public int java.lang.String.lastIndexOf(int,int)", "lastIndexOf");
        testedDeclaredMethods_jdk6.put("public int java.lang.String.lastIndexOf(java.lang.String)", "lastIndexOf");
        testedDeclaredMethods_jdk6.put("public int java.lang.String.lastIndexOf(java.lang.String,int)", "lastIndexOf");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.String.substring(int)", "substring");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.String.substring(int,int)", "substring");
        testedDeclaredMethods_jdk6.put("public java.lang.CharSequence java.lang.String.subSequence(int,int)", "subSequence");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.String.concat(java.lang.String)", "concat");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.String.replace(char,char)", "replace");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.String.replace(java.lang.CharSequence,java.lang.CharSequence)", "replace");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.String.matches(java.lang.String)", "matches");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.String.contains(java.lang.CharSequence)", "contains");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.String.replaceFirst(java.lang.String,java.lang.String)", "replaceFirst");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.String.replaceAll(java.lang.String,java.lang.String)", "replaceAll");
        testedDeclaredMethods_jdk6.put("public java.lang.String[] java.lang.String.split(java.lang.String,int)", "split");
        testedDeclaredMethods_jdk6.put("public java.lang.String[] java.lang.String.split(java.lang.String)", "split");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.String.toLowerCase(java.util.Locale)", "toLowerCase");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.String.toLowerCase()", "toLowerCase");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.String.toUpperCase()", "toUpperCase");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.String.toUpperCase(java.util.Locale)", "toUpperCase");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.String.trim()", "trim");
        testedDeclaredMethods_jdk6.put("public char[] java.lang.String.toCharArray()", "toCharArray");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.String.format(java.util.Locale,java.lang.String,java.lang.Object[])", "format");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.String.format(java.lang.String,java.lang.Object[])", "format");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.String.copyValueOf(char[],int,int)", "copyValueOf");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.String.copyValueOf(char[])", "copyValueOf");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.String.intern()", "intern");

        // map for methods declared in (Open)JDK7
        testedDeclaredMethods_jdk7.put("public boolean java.lang.String.equals(java.lang.Object)", "equals");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.String.toString()", "toString");
        testedDeclaredMethods_jdk7.put("public int java.lang.String.hashCode()", "hashCode");
        testedDeclaredMethods_jdk7.put("public int java.lang.String.compareTo(java.lang.Object)", "compareTo");
        testedDeclaredMethods_jdk7.put("public int java.lang.String.compareTo(java.lang.String)", "compareTo");
        testedDeclaredMethods_jdk7.put("public int java.lang.String.indexOf(java.lang.String,int)", "indexOf");
        testedDeclaredMethods_jdk7.put("public int java.lang.String.indexOf(int)", "indexOf");
        testedDeclaredMethods_jdk7.put("public int java.lang.String.indexOf(int,int)", "indexOf");
        testedDeclaredMethods_jdk7.put("static int java.lang.String.indexOf(char[],int,int,char[],int,int,int)", "indexOf");
        testedDeclaredMethods_jdk7.put("public int java.lang.String.indexOf(java.lang.String)", "indexOf");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(float)", "valueOf");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(double)", "valueOf");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(boolean)", "valueOf");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(char[],int,int)", "valueOf");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(char[])", "valueOf");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(java.lang.Object)", "valueOf");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(char)", "valueOf");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(int)", "valueOf");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(long)", "valueOf");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.String.contentEquals(java.lang.CharSequence)", "contentEquals");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.String.contentEquals(java.lang.StringBuffer)", "contentEquals");
        testedDeclaredMethods_jdk7.put("public char java.lang.String.charAt(int)", "charAt");
        testedDeclaredMethods_jdk7.put("private static void java.lang.String.checkBounds(byte[],int,int)", "checkBounds");
        testedDeclaredMethods_jdk7.put("public int java.lang.String.codePointAt(int)", "codePointAt");
        testedDeclaredMethods_jdk7.put("public int java.lang.String.codePointBefore(int)", "codePointBefore");
        testedDeclaredMethods_jdk7.put("public int java.lang.String.codePointCount(int,int)", "codePointCount");
        testedDeclaredMethods_jdk7.put("public int java.lang.String.compareToIgnoreCase(java.lang.String)", "compareToIgnoreCase");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.String.concat(java.lang.String)", "concat");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.String.contains(java.lang.CharSequence)", "contains");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.String.copyValueOf(char[])", "copyValueOf");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.String.copyValueOf(char[],int,int)", "copyValueOf");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.String.endsWith(java.lang.String)", "endsWith");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.String.equalsIgnoreCase(java.lang.String)", "equalsIgnoreCase");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.String.format(java.lang.String,java.lang.Object[])", "format");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.String.format(java.util.Locale,java.lang.String,java.lang.Object[])", "format");
        testedDeclaredMethods_jdk7.put("public byte[] java.lang.String.getBytes()", "getBytes");
        testedDeclaredMethods_jdk7.put("public byte[] java.lang.String.getBytes(java.lang.String) throws java.io.UnsupportedEncodingException", "getBytes");
        testedDeclaredMethods_jdk7.put("public void java.lang.String.getBytes(int,int,byte[],int)", "getBytes");
        testedDeclaredMethods_jdk7.put("public byte[] java.lang.String.getBytes(java.nio.charset.Charset)", "getBytes");
        testedDeclaredMethods_jdk7.put("void java.lang.String.getChars(char[],int)", "getChars");
        testedDeclaredMethods_jdk7.put("public void java.lang.String.getChars(int,int,char[],int)", "getChars");
        testedDeclaredMethods_jdk7.put("int java.lang.String.hash32()", "hash32");
        testedDeclaredMethods_jdk7.put("private int java.lang.String.indexOfSupplementary(int,int)", "indexOfSupplementary");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.String.intern()", "intern");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.String.isEmpty()", "isEmpty");
        testedDeclaredMethods_jdk7.put("public int java.lang.String.lastIndexOf(int)", "lastIndexOf");
        testedDeclaredMethods_jdk7.put("public int java.lang.String.lastIndexOf(int,int)", "lastIndexOf");
        testedDeclaredMethods_jdk7.put("public int java.lang.String.lastIndexOf(java.lang.String,int)", "lastIndexOf");
        testedDeclaredMethods_jdk7.put("public int java.lang.String.lastIndexOf(java.lang.String)", "lastIndexOf");
        testedDeclaredMethods_jdk7.put("static int java.lang.String.lastIndexOf(char[],int,int,char[],int,int,int)", "lastIndexOf");
        testedDeclaredMethods_jdk7.put("private int java.lang.String.lastIndexOfSupplementary(int,int)", "lastIndexOfSupplementary");
        testedDeclaredMethods_jdk7.put("public int java.lang.String.length()", "length");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.String.matches(java.lang.String)", "matches");
        testedDeclaredMethods_jdk7.put("public int java.lang.String.offsetByCodePoints(int,int)", "offsetByCodePoints");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.String.regionMatches(int,java.lang.String,int,int)", "regionMatches");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.String.regionMatches(boolean,int,java.lang.String,int,int)", "regionMatches");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.String.replace(char,char)", "replace");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.String.replace(java.lang.CharSequence,java.lang.CharSequence)", "replace");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.String.replaceAll(java.lang.String,java.lang.String)", "replaceAll");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.String.replaceFirst(java.lang.String,java.lang.String)", "replaceFirst");
        testedDeclaredMethods_jdk7.put("public java.lang.String[] java.lang.String.split(java.lang.String)", "split");
        testedDeclaredMethods_jdk7.put("public java.lang.String[] java.lang.String.split(java.lang.String,int)", "split");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.String.startsWith(java.lang.String)", "startsWith");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.String.startsWith(java.lang.String,int)", "startsWith");
        testedDeclaredMethods_jdk7.put("public java.lang.CharSequence java.lang.String.subSequence(int,int)", "subSequence");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.String.substring(int,int)", "substring");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.String.substring(int)", "substring");
        testedDeclaredMethods_jdk7.put("public char[] java.lang.String.toCharArray()", "toCharArray");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.String.toLowerCase()", "toLowerCase");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.String.toLowerCase(java.util.Locale)", "toLowerCase");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.String.toUpperCase(java.util.Locale)", "toUpperCase");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.String.toUpperCase()", "toUpperCase");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.String.trim()", "trim");

        // create instance of a class String
        final Object o = new String();

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

