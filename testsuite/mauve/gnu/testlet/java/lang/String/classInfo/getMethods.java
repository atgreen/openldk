// Test for method java.lang.String.getClass().getMethods()

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
 * Test for method java.lang.String.getClass().getMethods()
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
        testedMethods_jdk6.put("public boolean java.lang.String.equals(java.lang.Object)", "equals");
        testedMethods_jdk6.put("public java.lang.String java.lang.String.toString()", "toString");
        testedMethods_jdk6.put("public int java.lang.String.hashCode()", "hashCode");
        testedMethods_jdk6.put("public int java.lang.String.compareTo(java.lang.String)", "compareTo");
        testedMethods_jdk6.put("public int java.lang.String.compareTo(java.lang.Object)", "compareTo");
        testedMethods_jdk6.put("public int java.lang.String.indexOf(int,int)", "indexOf");
        testedMethods_jdk6.put("public int java.lang.String.indexOf(java.lang.String,int)", "indexOf");
        testedMethods_jdk6.put("public int java.lang.String.indexOf(java.lang.String)", "indexOf");
        testedMethods_jdk6.put("public int java.lang.String.indexOf(int)", "indexOf");
        testedMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(int)", "valueOf");
        testedMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(long)", "valueOf");
        testedMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(float)", "valueOf");
        testedMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(double)", "valueOf");
        testedMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(java.lang.Object)", "valueOf");
        testedMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(char[],int,int)", "valueOf");
        testedMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(char[])", "valueOf");
        testedMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(char)", "valueOf");
        testedMethods_jdk6.put("public static java.lang.String java.lang.String.valueOf(boolean)", "valueOf");
        testedMethods_jdk6.put("public int java.lang.String.length()", "length");
        testedMethods_jdk6.put("public boolean java.lang.String.isEmpty()", "isEmpty");
        testedMethods_jdk6.put("public char java.lang.String.charAt(int)", "charAt");
        testedMethods_jdk6.put("public int java.lang.String.codePointAt(int)", "codePointAt");
        testedMethods_jdk6.put("public int java.lang.String.codePointBefore(int)", "codePointBefore");
        testedMethods_jdk6.put("public int java.lang.String.codePointCount(int,int)", "codePointCount");
        testedMethods_jdk6.put("public int java.lang.String.offsetByCodePoints(int,int)", "offsetByCodePoints");
        testedMethods_jdk6.put("public void java.lang.String.getChars(int,int,char[],int)", "getChars");
        testedMethods_jdk6.put("public byte[] java.lang.String.getBytes(java.nio.charset.Charset)", "getBytes");
        testedMethods_jdk6.put("public byte[] java.lang.String.getBytes()", "getBytes");
        testedMethods_jdk6.put("public byte[] java.lang.String.getBytes(java.lang.String) throws java.io.UnsupportedEncodingException", "getBytes");
        testedMethods_jdk6.put("public void java.lang.String.getBytes(int,int,byte[],int)", "getBytes");
        testedMethods_jdk6.put("public boolean java.lang.String.contentEquals(java.lang.CharSequence)", "contentEquals");
        testedMethods_jdk6.put("public boolean java.lang.String.contentEquals(java.lang.StringBuffer)", "contentEquals");
        testedMethods_jdk6.put("public boolean java.lang.String.equalsIgnoreCase(java.lang.String)", "equalsIgnoreCase");
        testedMethods_jdk6.put("public int java.lang.String.compareToIgnoreCase(java.lang.String)", "compareToIgnoreCase");
        testedMethods_jdk6.put("public boolean java.lang.String.regionMatches(boolean,int,java.lang.String,int,int)", "regionMatches");
        testedMethods_jdk6.put("public boolean java.lang.String.regionMatches(int,java.lang.String,int,int)", "regionMatches");
        testedMethods_jdk6.put("public boolean java.lang.String.startsWith(java.lang.String)", "startsWith");
        testedMethods_jdk6.put("public boolean java.lang.String.startsWith(java.lang.String,int)", "startsWith");
        testedMethods_jdk6.put("public boolean java.lang.String.endsWith(java.lang.String)", "endsWith");
        testedMethods_jdk6.put("public int java.lang.String.lastIndexOf(int)", "lastIndexOf");
        testedMethods_jdk6.put("public int java.lang.String.lastIndexOf(int,int)", "lastIndexOf");
        testedMethods_jdk6.put("public int java.lang.String.lastIndexOf(java.lang.String)", "lastIndexOf");
        testedMethods_jdk6.put("public int java.lang.String.lastIndexOf(java.lang.String,int)", "lastIndexOf");
        testedMethods_jdk6.put("public java.lang.String java.lang.String.substring(int)", "substring");
        testedMethods_jdk6.put("public java.lang.String java.lang.String.substring(int,int)", "substring");
        testedMethods_jdk6.put("public java.lang.CharSequence java.lang.String.subSequence(int,int)", "subSequence");
        testedMethods_jdk6.put("public java.lang.String java.lang.String.concat(java.lang.String)", "concat");
        testedMethods_jdk6.put("public java.lang.String java.lang.String.replace(char,char)", "replace");
        testedMethods_jdk6.put("public java.lang.String java.lang.String.replace(java.lang.CharSequence,java.lang.CharSequence)", "replace");
        testedMethods_jdk6.put("public boolean java.lang.String.matches(java.lang.String)", "matches");
        testedMethods_jdk6.put("public boolean java.lang.String.contains(java.lang.CharSequence)", "contains");
        testedMethods_jdk6.put("public java.lang.String java.lang.String.replaceFirst(java.lang.String,java.lang.String)", "replaceFirst");
        testedMethods_jdk6.put("public java.lang.String java.lang.String.replaceAll(java.lang.String,java.lang.String)", "replaceAll");
        testedMethods_jdk6.put("public java.lang.String[] java.lang.String.split(java.lang.String,int)", "split");
        testedMethods_jdk6.put("public java.lang.String[] java.lang.String.split(java.lang.String)", "split");
        testedMethods_jdk6.put("public java.lang.String java.lang.String.toLowerCase(java.util.Locale)", "toLowerCase");
        testedMethods_jdk6.put("public java.lang.String java.lang.String.toLowerCase()", "toLowerCase");
        testedMethods_jdk6.put("public java.lang.String java.lang.String.toUpperCase()", "toUpperCase");
        testedMethods_jdk6.put("public java.lang.String java.lang.String.toUpperCase(java.util.Locale)", "toUpperCase");
        testedMethods_jdk6.put("public java.lang.String java.lang.String.trim()", "trim");
        testedMethods_jdk6.put("public char[] java.lang.String.toCharArray()", "toCharArray");
        testedMethods_jdk6.put("public static java.lang.String java.lang.String.format(java.util.Locale,java.lang.String,java.lang.Object[])", "format");
        testedMethods_jdk6.put("public static java.lang.String java.lang.String.format(java.lang.String,java.lang.Object[])", "format");
        testedMethods_jdk6.put("public static java.lang.String java.lang.String.copyValueOf(char[],int,int)", "copyValueOf");
        testedMethods_jdk6.put("public static java.lang.String java.lang.String.copyValueOf(char[])", "copyValueOf");
        testedMethods_jdk6.put("public java.lang.String java.lang.String.intern()", "intern");
        testedMethods_jdk6.put("public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public final void java.lang.Object.wait(long) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public final void java.lang.Object.wait() throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public final java.lang.Class java.lang.Object.getClass()", "getClass");
        testedMethods_jdk6.put("public final void java.lang.Object.notify()", "notify");
        testedMethods_jdk6.put("public final void java.lang.Object.notifyAll()", "notifyAll");

        // map for methods declared in (Open)JDK7
        testedMethods_jdk7.put("public boolean java.lang.String.equals(java.lang.Object)", "equals");
        testedMethods_jdk7.put("public java.lang.String java.lang.String.toString()", "toString");
        testedMethods_jdk7.put("public int java.lang.String.hashCode()", "hashCode");
        testedMethods_jdk7.put("public int java.lang.String.compareTo(java.lang.Object)", "compareTo");
        testedMethods_jdk7.put("public int java.lang.String.compareTo(java.lang.String)", "compareTo");
        testedMethods_jdk7.put("public int java.lang.String.indexOf(java.lang.String,int)", "indexOf");
        testedMethods_jdk7.put("public int java.lang.String.indexOf(int)", "indexOf");
        testedMethods_jdk7.put("public int java.lang.String.indexOf(int,int)", "indexOf");
        testedMethods_jdk7.put("public int java.lang.String.indexOf(java.lang.String)", "indexOf");
        testedMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(float)", "valueOf");
        testedMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(double)", "valueOf");
        testedMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(boolean)", "valueOf");
        testedMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(char[],int,int)", "valueOf");
        testedMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(char[])", "valueOf");
        testedMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(java.lang.Object)", "valueOf");
        testedMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(char)", "valueOf");
        testedMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(int)", "valueOf");
        testedMethods_jdk7.put("public static java.lang.String java.lang.String.valueOf(long)", "valueOf");
        testedMethods_jdk7.put("public boolean java.lang.String.contentEquals(java.lang.CharSequence)", "contentEquals");
        testedMethods_jdk7.put("public boolean java.lang.String.contentEquals(java.lang.StringBuffer)", "contentEquals");
        testedMethods_jdk7.put("public char java.lang.String.charAt(int)", "charAt");
        testedMethods_jdk7.put("public int java.lang.String.codePointAt(int)", "codePointAt");
        testedMethods_jdk7.put("public int java.lang.String.codePointBefore(int)", "codePointBefore");
        testedMethods_jdk7.put("public int java.lang.String.codePointCount(int,int)", "codePointCount");
        testedMethods_jdk7.put("public int java.lang.String.compareToIgnoreCase(java.lang.String)", "compareToIgnoreCase");
        testedMethods_jdk7.put("public java.lang.String java.lang.String.concat(java.lang.String)", "concat");
        testedMethods_jdk7.put("public boolean java.lang.String.contains(java.lang.CharSequence)", "contains");
        testedMethods_jdk7.put("public static java.lang.String java.lang.String.copyValueOf(char[])", "copyValueOf");
        testedMethods_jdk7.put("public static java.lang.String java.lang.String.copyValueOf(char[],int,int)", "copyValueOf");
        testedMethods_jdk7.put("public boolean java.lang.String.endsWith(java.lang.String)", "endsWith");
        testedMethods_jdk7.put("public boolean java.lang.String.equalsIgnoreCase(java.lang.String)", "equalsIgnoreCase");
        testedMethods_jdk7.put("public static java.lang.String java.lang.String.format(java.lang.String,java.lang.Object[])", "format");
        testedMethods_jdk7.put("public static java.lang.String java.lang.String.format(java.util.Locale,java.lang.String,java.lang.Object[])", "format");
        testedMethods_jdk7.put("public byte[] java.lang.String.getBytes()", "getBytes");
        testedMethods_jdk7.put("public byte[] java.lang.String.getBytes(java.lang.String) throws java.io.UnsupportedEncodingException", "getBytes");
        testedMethods_jdk7.put("public void java.lang.String.getBytes(int,int,byte[],int)", "getBytes");
        testedMethods_jdk7.put("public byte[] java.lang.String.getBytes(java.nio.charset.Charset)", "getBytes");
        testedMethods_jdk7.put("public void java.lang.String.getChars(int,int,char[],int)", "getChars");
        testedMethods_jdk7.put("public java.lang.String java.lang.String.intern()", "intern");
        testedMethods_jdk7.put("public boolean java.lang.String.isEmpty()", "isEmpty");
        testedMethods_jdk7.put("public int java.lang.String.lastIndexOf(int)", "lastIndexOf");
        testedMethods_jdk7.put("public int java.lang.String.lastIndexOf(int,int)", "lastIndexOf");
        testedMethods_jdk7.put("public int java.lang.String.lastIndexOf(java.lang.String,int)", "lastIndexOf");
        testedMethods_jdk7.put("public int java.lang.String.lastIndexOf(java.lang.String)", "lastIndexOf");
        testedMethods_jdk7.put("public int java.lang.String.length()", "length");
        testedMethods_jdk7.put("public boolean java.lang.String.matches(java.lang.String)", "matches");
        testedMethods_jdk7.put("public int java.lang.String.offsetByCodePoints(int,int)", "offsetByCodePoints");
        testedMethods_jdk7.put("public boolean java.lang.String.regionMatches(int,java.lang.String,int,int)", "regionMatches");
        testedMethods_jdk7.put("public boolean java.lang.String.regionMatches(boolean,int,java.lang.String,int,int)", "regionMatches");
        testedMethods_jdk7.put("public java.lang.String java.lang.String.replace(char,char)", "replace");
        testedMethods_jdk7.put("public java.lang.String java.lang.String.replace(java.lang.CharSequence,java.lang.CharSequence)", "replace");
        testedMethods_jdk7.put("public java.lang.String java.lang.String.replaceAll(java.lang.String,java.lang.String)", "replaceAll");
        testedMethods_jdk7.put("public java.lang.String java.lang.String.replaceFirst(java.lang.String,java.lang.String)", "replaceFirst");
        testedMethods_jdk7.put("public java.lang.String[] java.lang.String.split(java.lang.String)", "split");
        testedMethods_jdk7.put("public java.lang.String[] java.lang.String.split(java.lang.String,int)", "split");
        testedMethods_jdk7.put("public boolean java.lang.String.startsWith(java.lang.String)", "startsWith");
        testedMethods_jdk7.put("public boolean java.lang.String.startsWith(java.lang.String,int)", "startsWith");
        testedMethods_jdk7.put("public java.lang.CharSequence java.lang.String.subSequence(int,int)", "subSequence");
        testedMethods_jdk7.put("public java.lang.String java.lang.String.substring(int,int)", "substring");
        testedMethods_jdk7.put("public java.lang.String java.lang.String.substring(int)", "substring");
        testedMethods_jdk7.put("public char[] java.lang.String.toCharArray()", "toCharArray");
        testedMethods_jdk7.put("public java.lang.String java.lang.String.toLowerCase()", "toLowerCase");
        testedMethods_jdk7.put("public java.lang.String java.lang.String.toLowerCase(java.util.Locale)", "toLowerCase");
        testedMethods_jdk7.put("public java.lang.String java.lang.String.toUpperCase(java.util.Locale)", "toUpperCase");
        testedMethods_jdk7.put("public java.lang.String java.lang.String.toUpperCase()", "toUpperCase");
        testedMethods_jdk7.put("public java.lang.String java.lang.String.trim()", "trim");
        testedMethods_jdk7.put("public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public final void java.lang.Object.wait(long) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public final void java.lang.Object.wait() throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public final java.lang.Class java.lang.Object.getClass()", "getClass");
        testedMethods_jdk7.put("public final void java.lang.Object.notify()", "notify");
        testedMethods_jdk7.put("public final void java.lang.Object.notifyAll()", "notifyAll");

        // create instance of a class String
        final Object o = new String();

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

