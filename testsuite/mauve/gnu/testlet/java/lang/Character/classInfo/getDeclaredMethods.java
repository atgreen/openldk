// Test for method java.lang.Character.getClass().getDeclaredMethods()

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
 * Test for method java.lang.Character.getClass().getDeclaredMethods()
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
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isJavaIdentifierStart(int)", "isJavaIdentifierStart");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isJavaIdentifierStart(char)", "isJavaIdentifierStart");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isJavaIdentifierPart(int)", "isJavaIdentifierPart");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isJavaIdentifierPart(char)", "isJavaIdentifierPart");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Character.equals(java.lang.Object)", "equals");
        testedDeclaredMethods_jdk6.put("public static java.lang.String java.lang.Character.toString(char)", "toString");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.Character.toString()", "toString");
        testedDeclaredMethods_jdk6.put("public int java.lang.Character.hashCode()", "hashCode");
        testedDeclaredMethods_jdk6.put("public static char java.lang.Character.reverseBytes(char)", "reverseBytes");
        testedDeclaredMethods_jdk6.put("public int java.lang.Character.compareTo(java.lang.Object)", "compareTo");
        testedDeclaredMethods_jdk6.put("public int java.lang.Character.compareTo(java.lang.Character)", "compareTo");
        testedDeclaredMethods_jdk6.put("public char java.lang.Character.charValue()", "charValue");
        testedDeclaredMethods_jdk6.put("public static java.lang.Character java.lang.Character.valueOf(char)", "valueOf");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.codePointAt(char[],int,int)", "codePointAt");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.codePointAt(char[],int)", "codePointAt");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.codePointAt(java.lang.CharSequence,int)", "codePointAt");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.codePointBefore(char[],int)", "codePointBefore");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.codePointBefore(char[],int,int)", "codePointBefore");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.codePointBefore(java.lang.CharSequence,int)", "codePointBefore");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.codePointCount(java.lang.CharSequence,int,int)", "codePointCount");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.codePointCount(char[],int,int)", "codePointCount");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.offsetByCodePoints(char[],int,int,int,int)", "offsetByCodePoints");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.offsetByCodePoints(java.lang.CharSequence,int,int)", "offsetByCodePoints");
        testedDeclaredMethods_jdk6.put("public static char java.lang.Character.toLowerCase(char)", "toLowerCase");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.toLowerCase(int)", "toLowerCase");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.toUpperCase(int)", "toUpperCase");
        testedDeclaredMethods_jdk6.put("public static char java.lang.Character.toUpperCase(char)", "toUpperCase");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isSupplementaryCodePoint(int)", "isSupplementaryCodePoint");
        testedDeclaredMethods_jdk6.put("static void java.lang.Character.toSurrogates(int,char[],int)", "toSurrogates");
        testedDeclaredMethods_jdk6.put("static int java.lang.Character.codePointAtImpl(char[],int,int)", "codePointAtImpl");
        testedDeclaredMethods_jdk6.put("static int java.lang.Character.codePointBeforeImpl(char[],int,int)", "codePointBeforeImpl");
        testedDeclaredMethods_jdk6.put("static int java.lang.Character.codePointCountImpl(char[],int,int)", "codePointCountImpl");
        testedDeclaredMethods_jdk6.put("static int java.lang.Character.offsetByCodePointsImpl(char[],int,int,int,int)", "offsetByCodePointsImpl");
        testedDeclaredMethods_jdk6.put("public static char[] java.lang.Character.toChars(int)", "toChars");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.toChars(int,char[],int)", "toChars");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.charCount(int)", "charCount");
        testedDeclaredMethods_jdk6.put("static int java.lang.Character.toUpperCaseEx(int)", "toUpperCaseEx");
        testedDeclaredMethods_jdk6.put("static char[] java.lang.Character.toUpperCaseCharArray(int)", "toUpperCaseCharArray");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.getType(char)", "getType");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.getType(int)", "getType");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isValidCodePoint(int)", "isValidCodePoint");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isLowSurrogate(char)", "isLowSurrogate");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isHighSurrogate(char)", "isHighSurrogate");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isSurrogatePair(char,char)", "isSurrogatePair");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.toCodePoint(char,char)", "toCodePoint");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isLowerCase(char)", "isLowerCase");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isLowerCase(int)", "isLowerCase");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isUpperCase(int)", "isUpperCase");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isUpperCase(char)", "isUpperCase");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isTitleCase(int)", "isTitleCase");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isTitleCase(char)", "isTitleCase");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isDigit(char)", "isDigit");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isDigit(int)", "isDigit");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isDefined(int)", "isDefined");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isDefined(char)", "isDefined");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isLetter(int)", "isLetter");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isLetter(char)", "isLetter");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isLetterOrDigit(char)", "isLetterOrDigit");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isLetterOrDigit(int)", "isLetterOrDigit");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isJavaLetter(char)", "isJavaLetter");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isJavaLetterOrDigit(char)", "isJavaLetterOrDigit");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isUnicodeIdentifierStart(int)", "isUnicodeIdentifierStart");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isUnicodeIdentifierStart(char)", "isUnicodeIdentifierStart");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isUnicodeIdentifierPart(int)", "isUnicodeIdentifierPart");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isUnicodeIdentifierPart(char)", "isUnicodeIdentifierPart");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isIdentifierIgnorable(char)", "isIdentifierIgnorable");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isIdentifierIgnorable(int)", "isIdentifierIgnorable");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.toTitleCase(int)", "toTitleCase");
        testedDeclaredMethods_jdk6.put("public static char java.lang.Character.toTitleCase(char)", "toTitleCase");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.digit(char,int)", "digit");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.digit(int,int)", "digit");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.getNumericValue(int)", "getNumericValue");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Character.getNumericValue(char)", "getNumericValue");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isSpace(char)", "isSpace");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isSpaceChar(int)", "isSpaceChar");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isSpaceChar(char)", "isSpaceChar");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isWhitespace(char)", "isWhitespace");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isWhitespace(int)", "isWhitespace");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isISOControl(int)", "isISOControl");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isISOControl(char)", "isISOControl");
        testedDeclaredMethods_jdk6.put("public static char java.lang.Character.forDigit(int,int)", "forDigit");
        testedDeclaredMethods_jdk6.put("public static byte java.lang.Character.getDirectionality(int)", "getDirectionality");
        testedDeclaredMethods_jdk6.put("public static byte java.lang.Character.getDirectionality(char)", "getDirectionality");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isMirrored(char)", "isMirrored");
        testedDeclaredMethods_jdk6.put("public static boolean java.lang.Character.isMirrored(int)", "isMirrored");

        // map for methods declared in (Open)JDK7
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isJavaIdentifierStart(char)", "isJavaIdentifierStart");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isJavaIdentifierStart(int)", "isJavaIdentifierStart");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isJavaIdentifierPart(int)", "isJavaIdentifierPart");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isJavaIdentifierPart(char)", "isJavaIdentifierPart");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Character.equals(java.lang.Object)", "equals");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.Character.toString(char)", "toString");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.Character.toString()", "toString");
        testedDeclaredMethods_jdk7.put("public int java.lang.Character.hashCode()", "hashCode");
        testedDeclaredMethods_jdk7.put("public static char java.lang.Character.reverseBytes(char)", "reverseBytes");
        testedDeclaredMethods_jdk7.put("public int java.lang.Character.compareTo(java.lang.Object)", "compareTo");
        testedDeclaredMethods_jdk7.put("public int java.lang.Character.compareTo(java.lang.Character)", "compareTo");
        testedDeclaredMethods_jdk7.put("public char java.lang.Character.charValue()", "charValue");
        testedDeclaredMethods_jdk7.put("public static java.lang.Character java.lang.Character.valueOf(char)", "valueOf");
        testedDeclaredMethods_jdk7.put("public static java.lang.String java.lang.Character.getName(int)", "getName");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.charCount(int)", "charCount");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.codePointAt(char[],int,int)", "codePointAt");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.codePointAt(java.lang.CharSequence,int)", "codePointAt");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.codePointAt(char[],int)", "codePointAt");
        testedDeclaredMethods_jdk7.put("static int java.lang.Character.codePointAtImpl(char[],int,int)", "codePointAtImpl");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.codePointBefore(char[],int,int)", "codePointBefore");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.codePointBefore(char[],int)", "codePointBefore");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.codePointBefore(java.lang.CharSequence,int)", "codePointBefore");
        testedDeclaredMethods_jdk7.put("static int java.lang.Character.codePointBeforeImpl(char[],int,int)", "codePointBeforeImpl");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.codePointCount(char[],int,int)", "codePointCount");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.codePointCount(java.lang.CharSequence,int,int)", "codePointCount");
        testedDeclaredMethods_jdk7.put("static int java.lang.Character.codePointCountImpl(char[],int,int)", "codePointCountImpl");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.compare(char,char)", "compare");
        testedDeclaredMethods_jdk7.put("public static char java.lang.Character.highSurrogate(int)", "highSurrogate");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isBmpCodePoint(int)", "isBmpCodePoint");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isValidCodePoint(int)", "isValidCodePoint");
        testedDeclaredMethods_jdk7.put("public static char java.lang.Character.lowSurrogate(int)", "lowSurrogate");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.offsetByCodePoints(char[],int,int,int,int)", "offsetByCodePoints");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.offsetByCodePoints(java.lang.CharSequence,int,int)", "offsetByCodePoints");
        testedDeclaredMethods_jdk7.put("static int java.lang.Character.offsetByCodePointsImpl(char[],int,int,int,int)", "offsetByCodePointsImpl");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.toChars(int,char[],int)", "toChars");
        testedDeclaredMethods_jdk7.put("public static char[] java.lang.Character.toChars(int)", "toChars");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.toLowerCase(int)", "toLowerCase");
        testedDeclaredMethods_jdk7.put("public static char java.lang.Character.toLowerCase(char)", "toLowerCase");
        testedDeclaredMethods_jdk7.put("static void java.lang.Character.toSurrogates(int,char[],int)", "toSurrogates");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.toUpperCase(int)", "toUpperCase");
        testedDeclaredMethods_jdk7.put("public static char java.lang.Character.toUpperCase(char)", "toUpperCase");
        testedDeclaredMethods_jdk7.put("static char[] java.lang.Character.toUpperCaseCharArray(int)", "toUpperCaseCharArray");
        testedDeclaredMethods_jdk7.put("static int java.lang.Character.toUpperCaseEx(int)", "toUpperCaseEx");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.getType(int)", "getType");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.getType(char)", "getType");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isHighSurrogate(char)", "isHighSurrogate");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isLowSurrogate(char)", "isLowSurrogate");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.digit(char,int)", "digit");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.digit(int,int)", "digit");
        testedDeclaredMethods_jdk7.put("public static char java.lang.Character.forDigit(int,int)", "forDigit");
        testedDeclaredMethods_jdk7.put("public static byte java.lang.Character.getDirectionality(int)", "getDirectionality");
        testedDeclaredMethods_jdk7.put("public static byte java.lang.Character.getDirectionality(char)", "getDirectionality");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.getNumericValue(char)", "getNumericValue");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.getNumericValue(int)", "getNumericValue");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isAlphabetic(int)", "isAlphabetic");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isDefined(char)", "isDefined");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isDefined(int)", "isDefined");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isDigit(char)", "isDigit");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isDigit(int)", "isDigit");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isISOControl(int)", "isISOControl");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isISOControl(char)", "isISOControl");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isIdentifierIgnorable(char)", "isIdentifierIgnorable");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isIdentifierIgnorable(int)", "isIdentifierIgnorable");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isIdeographic(int)", "isIdeographic");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isJavaLetter(char)", "isJavaLetter");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isJavaLetterOrDigit(char)", "isJavaLetterOrDigit");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isLetter(char)", "isLetter");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isLetter(int)", "isLetter");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isLetterOrDigit(char)", "isLetterOrDigit");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isLetterOrDigit(int)", "isLetterOrDigit");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isLowerCase(int)", "isLowerCase");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isLowerCase(char)", "isLowerCase");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isMirrored(char)", "isMirrored");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isMirrored(int)", "isMirrored");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isSpace(char)", "isSpace");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isSpaceChar(char)", "isSpaceChar");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isSpaceChar(int)", "isSpaceChar");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isSupplementaryCodePoint(int)", "isSupplementaryCodePoint");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isSurrogate(char)", "isSurrogate");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isSurrogatePair(char,char)", "isSurrogatePair");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isTitleCase(int)", "isTitleCase");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isTitleCase(char)", "isTitleCase");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isUnicodeIdentifierPart(int)", "isUnicodeIdentifierPart");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isUnicodeIdentifierPart(char)", "isUnicodeIdentifierPart");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isUnicodeIdentifierStart(char)", "isUnicodeIdentifierStart");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isUnicodeIdentifierStart(int)", "isUnicodeIdentifierStart");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isUpperCase(int)", "isUpperCase");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isUpperCase(char)", "isUpperCase");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isWhitespace(int)", "isWhitespace");
        testedDeclaredMethods_jdk7.put("public static boolean java.lang.Character.isWhitespace(char)", "isWhitespace");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.toCodePoint(char,char)", "toCodePoint");
        testedDeclaredMethods_jdk7.put("public static char java.lang.Character.toTitleCase(char)", "toTitleCase");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Character.toTitleCase(int)", "toTitleCase");

        // create instance of a class Character
        final Object o = new Character('!');

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

