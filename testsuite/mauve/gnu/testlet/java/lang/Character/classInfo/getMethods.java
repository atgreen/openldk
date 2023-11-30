// Test for method java.lang.Character.getClass().getMethods()

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

package gnu.testlet.java.lang.Character.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.Character;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.lang.Character.getClass().getMethods()
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
        testedMethods_jdk6.put("public static boolean java.lang.Character.isJavaIdentifierStart(int)", "isJavaIdentifierStart");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isJavaIdentifierStart(char)", "isJavaIdentifierStart");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isJavaIdentifierPart(int)", "isJavaIdentifierPart");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isJavaIdentifierPart(char)", "isJavaIdentifierPart");
        testedMethods_jdk6.put("public boolean java.lang.Character.equals(java.lang.Object)", "equals");
        testedMethods_jdk6.put("public static java.lang.String java.lang.Character.toString(char)", "toString");
        testedMethods_jdk6.put("public java.lang.String java.lang.Character.toString()", "toString");
        testedMethods_jdk6.put("public int java.lang.Character.hashCode()", "hashCode");
        testedMethods_jdk6.put("public static char java.lang.Character.reverseBytes(char)", "reverseBytes");
        testedMethods_jdk6.put("public int java.lang.Character.compareTo(java.lang.Object)", "compareTo");
        testedMethods_jdk6.put("public int java.lang.Character.compareTo(java.lang.Character)", "compareTo");
        testedMethods_jdk6.put("public char java.lang.Character.charValue()", "charValue");
        testedMethods_jdk6.put("public static java.lang.Character java.lang.Character.valueOf(char)", "valueOf");
        testedMethods_jdk6.put("public static int java.lang.Character.codePointAt(char[],int,int)", "codePointAt");
        testedMethods_jdk6.put("public static int java.lang.Character.codePointAt(char[],int)", "codePointAt");
        testedMethods_jdk6.put("public static int java.lang.Character.codePointAt(java.lang.CharSequence,int)", "codePointAt");
        testedMethods_jdk6.put("public static int java.lang.Character.codePointBefore(char[],int)", "codePointBefore");
        testedMethods_jdk6.put("public static int java.lang.Character.codePointBefore(char[],int,int)", "codePointBefore");
        testedMethods_jdk6.put("public static int java.lang.Character.codePointBefore(java.lang.CharSequence,int)", "codePointBefore");
        testedMethods_jdk6.put("public static int java.lang.Character.codePointCount(java.lang.CharSequence,int,int)", "codePointCount");
        testedMethods_jdk6.put("public static int java.lang.Character.codePointCount(char[],int,int)", "codePointCount");
        testedMethods_jdk6.put("public static int java.lang.Character.offsetByCodePoints(char[],int,int,int,int)", "offsetByCodePoints");
        testedMethods_jdk6.put("public static int java.lang.Character.offsetByCodePoints(java.lang.CharSequence,int,int)", "offsetByCodePoints");
        testedMethods_jdk6.put("public static char java.lang.Character.toLowerCase(char)", "toLowerCase");
        testedMethods_jdk6.put("public static int java.lang.Character.toLowerCase(int)", "toLowerCase");
        testedMethods_jdk6.put("public static int java.lang.Character.toUpperCase(int)", "toUpperCase");
        testedMethods_jdk6.put("public static char java.lang.Character.toUpperCase(char)", "toUpperCase");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isSupplementaryCodePoint(int)", "isSupplementaryCodePoint");
        testedMethods_jdk6.put("public static char[] java.lang.Character.toChars(int)", "toChars");
        testedMethods_jdk6.put("public static int java.lang.Character.toChars(int,char[],int)", "toChars");
        testedMethods_jdk6.put("public static int java.lang.Character.charCount(int)", "charCount");
        testedMethods_jdk6.put("public static int java.lang.Character.getType(char)", "getType");
        testedMethods_jdk6.put("public static int java.lang.Character.getType(int)", "getType");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isValidCodePoint(int)", "isValidCodePoint");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isLowSurrogate(char)", "isLowSurrogate");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isHighSurrogate(char)", "isHighSurrogate");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isSurrogatePair(char,char)", "isSurrogatePair");
        testedMethods_jdk6.put("public static int java.lang.Character.toCodePoint(char,char)", "toCodePoint");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isLowerCase(char)", "isLowerCase");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isLowerCase(int)", "isLowerCase");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isUpperCase(int)", "isUpperCase");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isUpperCase(char)", "isUpperCase");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isTitleCase(int)", "isTitleCase");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isTitleCase(char)", "isTitleCase");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isDigit(char)", "isDigit");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isDigit(int)", "isDigit");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isDefined(int)", "isDefined");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isDefined(char)", "isDefined");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isLetter(int)", "isLetter");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isLetter(char)", "isLetter");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isLetterOrDigit(char)", "isLetterOrDigit");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isLetterOrDigit(int)", "isLetterOrDigit");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isJavaLetter(char)", "isJavaLetter");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isJavaLetterOrDigit(char)", "isJavaLetterOrDigit");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isUnicodeIdentifierStart(int)", "isUnicodeIdentifierStart");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isUnicodeIdentifierStart(char)", "isUnicodeIdentifierStart");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isUnicodeIdentifierPart(int)", "isUnicodeIdentifierPart");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isUnicodeIdentifierPart(char)", "isUnicodeIdentifierPart");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isIdentifierIgnorable(char)", "isIdentifierIgnorable");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isIdentifierIgnorable(int)", "isIdentifierIgnorable");
        testedMethods_jdk6.put("public static int java.lang.Character.toTitleCase(int)", "toTitleCase");
        testedMethods_jdk6.put("public static char java.lang.Character.toTitleCase(char)", "toTitleCase");
        testedMethods_jdk6.put("public static int java.lang.Character.digit(char,int)", "digit");
        testedMethods_jdk6.put("public static int java.lang.Character.digit(int,int)", "digit");
        testedMethods_jdk6.put("public static int java.lang.Character.getNumericValue(int)", "getNumericValue");
        testedMethods_jdk6.put("public static int java.lang.Character.getNumericValue(char)", "getNumericValue");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isSpace(char)", "isSpace");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isSpaceChar(int)", "isSpaceChar");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isSpaceChar(char)", "isSpaceChar");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isWhitespace(char)", "isWhitespace");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isWhitespace(int)", "isWhitespace");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isISOControl(int)", "isISOControl");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isISOControl(char)", "isISOControl");
        testedMethods_jdk6.put("public static char java.lang.Character.forDigit(int,int)", "forDigit");
        testedMethods_jdk6.put("public static byte java.lang.Character.getDirectionality(int)", "getDirectionality");
        testedMethods_jdk6.put("public static byte java.lang.Character.getDirectionality(char)", "getDirectionality");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isMirrored(char)", "isMirrored");
        testedMethods_jdk6.put("public static boolean java.lang.Character.isMirrored(int)", "isMirrored");
        testedMethods_jdk6.put("public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public final void java.lang.Object.wait(long) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public final void java.lang.Object.wait() throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public final java.lang.Class java.lang.Object.getClass()", "getClass");
        testedMethods_jdk6.put("public final void java.lang.Object.notify()", "notify");
        testedMethods_jdk6.put("public final void java.lang.Object.notifyAll()", "notifyAll");

        // map for methods declared in (Open)JDK7
        testedMethods_jdk7.put("public static boolean java.lang.Character.isJavaIdentifierStart(char)", "isJavaIdentifierStart");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isJavaIdentifierStart(int)", "isJavaIdentifierStart");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isJavaIdentifierPart(int)", "isJavaIdentifierPart");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isJavaIdentifierPart(char)", "isJavaIdentifierPart");
        testedMethods_jdk7.put("public boolean java.lang.Character.equals(java.lang.Object)", "equals");
        testedMethods_jdk7.put("public static java.lang.String java.lang.Character.toString(char)", "toString");
        testedMethods_jdk7.put("public java.lang.String java.lang.Character.toString()", "toString");
        testedMethods_jdk7.put("public int java.lang.Character.hashCode()", "hashCode");
        testedMethods_jdk7.put("public static char java.lang.Character.reverseBytes(char)", "reverseBytes");
        testedMethods_jdk7.put("public int java.lang.Character.compareTo(java.lang.Object)", "compareTo");
        testedMethods_jdk7.put("public int java.lang.Character.compareTo(java.lang.Character)", "compareTo");
        testedMethods_jdk7.put("public char java.lang.Character.charValue()", "charValue");
        testedMethods_jdk7.put("public static java.lang.Character java.lang.Character.valueOf(char)", "valueOf");
        testedMethods_jdk7.put("public static java.lang.String java.lang.Character.getName(int)", "getName");
        testedMethods_jdk7.put("public static int java.lang.Character.charCount(int)", "charCount");
        testedMethods_jdk7.put("public static int java.lang.Character.codePointAt(char[],int,int)", "codePointAt");
        testedMethods_jdk7.put("public static int java.lang.Character.codePointAt(java.lang.CharSequence,int)", "codePointAt");
        testedMethods_jdk7.put("public static int java.lang.Character.codePointAt(char[],int)", "codePointAt");
        testedMethods_jdk7.put("public static int java.lang.Character.codePointBefore(char[],int,int)", "codePointBefore");
        testedMethods_jdk7.put("public static int java.lang.Character.codePointBefore(char[],int)", "codePointBefore");
        testedMethods_jdk7.put("public static int java.lang.Character.codePointBefore(java.lang.CharSequence,int)", "codePointBefore");
        testedMethods_jdk7.put("public static int java.lang.Character.codePointCount(char[],int,int)", "codePointCount");
        testedMethods_jdk7.put("public static int java.lang.Character.codePointCount(java.lang.CharSequence,int,int)", "codePointCount");
        testedMethods_jdk7.put("public static int java.lang.Character.compare(char,char)", "compare");
        testedMethods_jdk7.put("public static char java.lang.Character.highSurrogate(int)", "highSurrogate");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isBmpCodePoint(int)", "isBmpCodePoint");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isValidCodePoint(int)", "isValidCodePoint");
        testedMethods_jdk7.put("public static char java.lang.Character.lowSurrogate(int)", "lowSurrogate");
        testedMethods_jdk7.put("public static int java.lang.Character.offsetByCodePoints(char[],int,int,int,int)", "offsetByCodePoints");
        testedMethods_jdk7.put("public static int java.lang.Character.offsetByCodePoints(java.lang.CharSequence,int,int)", "offsetByCodePoints");
        testedMethods_jdk7.put("public static int java.lang.Character.toChars(int,char[],int)", "toChars");
        testedMethods_jdk7.put("public static char[] java.lang.Character.toChars(int)", "toChars");
        testedMethods_jdk7.put("public static int java.lang.Character.toLowerCase(int)", "toLowerCase");
        testedMethods_jdk7.put("public static char java.lang.Character.toLowerCase(char)", "toLowerCase");
        testedMethods_jdk7.put("public static int java.lang.Character.toUpperCase(int)", "toUpperCase");
        testedMethods_jdk7.put("public static char java.lang.Character.toUpperCase(char)", "toUpperCase");
        testedMethods_jdk7.put("public static int java.lang.Character.getType(int)", "getType");
        testedMethods_jdk7.put("public static int java.lang.Character.getType(char)", "getType");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isHighSurrogate(char)", "isHighSurrogate");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isLowSurrogate(char)", "isLowSurrogate");
        testedMethods_jdk7.put("public static int java.lang.Character.digit(char,int)", "digit");
        testedMethods_jdk7.put("public static int java.lang.Character.digit(int,int)", "digit");
        testedMethods_jdk7.put("public static char java.lang.Character.forDigit(int,int)", "forDigit");
        testedMethods_jdk7.put("public static byte java.lang.Character.getDirectionality(int)", "getDirectionality");
        testedMethods_jdk7.put("public static byte java.lang.Character.getDirectionality(char)", "getDirectionality");
        testedMethods_jdk7.put("public static int java.lang.Character.getNumericValue(char)", "getNumericValue");
        testedMethods_jdk7.put("public static int java.lang.Character.getNumericValue(int)", "getNumericValue");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isAlphabetic(int)", "isAlphabetic");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isDefined(char)", "isDefined");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isDefined(int)", "isDefined");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isDigit(char)", "isDigit");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isDigit(int)", "isDigit");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isISOControl(int)", "isISOControl");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isISOControl(char)", "isISOControl");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isIdentifierIgnorable(char)", "isIdentifierIgnorable");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isIdentifierIgnorable(int)", "isIdentifierIgnorable");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isIdeographic(int)", "isIdeographic");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isJavaLetter(char)", "isJavaLetter");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isJavaLetterOrDigit(char)", "isJavaLetterOrDigit");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isLetter(char)", "isLetter");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isLetter(int)", "isLetter");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isLetterOrDigit(char)", "isLetterOrDigit");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isLetterOrDigit(int)", "isLetterOrDigit");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isLowerCase(int)", "isLowerCase");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isLowerCase(char)", "isLowerCase");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isMirrored(char)", "isMirrored");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isMirrored(int)", "isMirrored");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isSpace(char)", "isSpace");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isSpaceChar(char)", "isSpaceChar");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isSpaceChar(int)", "isSpaceChar");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isSupplementaryCodePoint(int)", "isSupplementaryCodePoint");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isSurrogate(char)", "isSurrogate");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isSurrogatePair(char,char)", "isSurrogatePair");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isTitleCase(int)", "isTitleCase");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isTitleCase(char)", "isTitleCase");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isUnicodeIdentifierPart(int)", "isUnicodeIdentifierPart");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isUnicodeIdentifierPart(char)", "isUnicodeIdentifierPart");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isUnicodeIdentifierStart(char)", "isUnicodeIdentifierStart");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isUnicodeIdentifierStart(int)", "isUnicodeIdentifierStart");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isUpperCase(int)", "isUpperCase");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isUpperCase(char)", "isUpperCase");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isWhitespace(int)", "isWhitespace");
        testedMethods_jdk7.put("public static boolean java.lang.Character.isWhitespace(char)", "isWhitespace");
        testedMethods_jdk7.put("public static int java.lang.Character.toCodePoint(char,char)", "toCodePoint");
        testedMethods_jdk7.put("public static char java.lang.Character.toTitleCase(char)", "toTitleCase");
        testedMethods_jdk7.put("public static int java.lang.Character.toTitleCase(int)", "toTitleCase");
        testedMethods_jdk7.put("public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public final void java.lang.Object.wait(long) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public final void java.lang.Object.wait() throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public final java.lang.Class java.lang.Object.getClass()", "getClass");
        testedMethods_jdk7.put("public final void java.lang.Object.notify()", "notify");
        testedMethods_jdk7.put("public final void java.lang.Object.notifyAll()", "notifyAll");

        // create instance of a class Character
        final Object o = new Character('!');

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

