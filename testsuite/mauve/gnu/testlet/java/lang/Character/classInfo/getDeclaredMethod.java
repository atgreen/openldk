// Test for method java.lang.Character.getClass().getDeclaredMethod()

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
 * Test for method java.lang.Character.getClass().getDeclaredMethod()
 */
public class getDeclaredMethod implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // following declared methods should exist
        Map<String, Class[]> methodsThatShouldExist_jdk6 = new HashMap<String, Class[]>();
        methodsThatShouldExist_jdk6.put("isJavaIdentifierStart", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("isJavaIdentifierStart", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isJavaIdentifierPart", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("isJavaIdentifierPart", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("equals", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk6.put("toString", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("toString", new Class[] {});
        methodsThatShouldExist_jdk6.put("hashCode", new Class[] {});
        methodsThatShouldExist_jdk6.put("reverseBytes", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("compareTo", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk6.put("compareTo", new Class[] {java.lang.Character.class});
        methodsThatShouldExist_jdk6.put("charValue", new Class[] {});
        methodsThatShouldExist_jdk6.put("valueOf", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("codePointAt", new Class[] {(new char[1]).getClass(), int.class, int.class});
        methodsThatShouldExist_jdk6.put("codePointAt", new Class[] {(new char[1]).getClass(), int.class});
        methodsThatShouldExist_jdk6.put("codePointAt", new Class[] {java.lang.CharSequence.class, int.class});
        methodsThatShouldExist_jdk6.put("codePointBefore", new Class[] {(new char[1]).getClass(), int.class});
        methodsThatShouldExist_jdk6.put("codePointBefore", new Class[] {(new char[1]).getClass(), int.class, int.class});
        methodsThatShouldExist_jdk6.put("codePointBefore", new Class[] {java.lang.CharSequence.class, int.class});
        methodsThatShouldExist_jdk6.put("codePointCount", new Class[] {java.lang.CharSequence.class, int.class, int.class});
        methodsThatShouldExist_jdk6.put("codePointCount", new Class[] {(new char[1]).getClass(), int.class, int.class});
        methodsThatShouldExist_jdk6.put("offsetByCodePoints", new Class[] {(new char[1]).getClass(), int.class, int.class, int.class, int.class});
        methodsThatShouldExist_jdk6.put("offsetByCodePoints", new Class[] {java.lang.CharSequence.class, int.class, int.class});
        methodsThatShouldExist_jdk6.put("toLowerCase", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("toLowerCase", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("toUpperCase", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("toUpperCase", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isSupplementaryCodePoint", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("toSurrogates", new Class[] {int.class, (new char[1]).getClass(), int.class});
        methodsThatShouldExist_jdk6.put("codePointAtImpl", new Class[] {(new char[1]).getClass(), int.class, int.class});
        methodsThatShouldExist_jdk6.put("codePointBeforeImpl", new Class[] {(new char[1]).getClass(), int.class, int.class});
        methodsThatShouldExist_jdk6.put("codePointCountImpl", new Class[] {(new char[1]).getClass(), int.class, int.class});
        methodsThatShouldExist_jdk6.put("offsetByCodePointsImpl", new Class[] {(new char[1]).getClass(), int.class, int.class, int.class, int.class});
        methodsThatShouldExist_jdk6.put("toChars", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("toChars", new Class[] {int.class, (new char[1]).getClass(), int.class});
        methodsThatShouldExist_jdk6.put("charCount", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("toUpperCaseEx", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("toUpperCaseCharArray", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("getType", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("getType", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("isValidCodePoint", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("isLowSurrogate", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isHighSurrogate", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isSurrogatePair", new Class[] {char.class, char.class});
        methodsThatShouldExist_jdk6.put("toCodePoint", new Class[] {char.class, char.class});
        methodsThatShouldExist_jdk6.put("isLowerCase", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isLowerCase", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("isUpperCase", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("isUpperCase", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isTitleCase", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("isTitleCase", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isDigit", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isDigit", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("isDefined", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("isDefined", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isLetter", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("isLetter", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isLetterOrDigit", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isLetterOrDigit", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("isJavaLetter", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isJavaLetterOrDigit", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isUnicodeIdentifierStart", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("isUnicodeIdentifierStart", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isUnicodeIdentifierPart", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("isUnicodeIdentifierPart", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isIdentifierIgnorable", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isIdentifierIgnorable", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("toTitleCase", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("toTitleCase", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("digit", new Class[] {char.class, int.class});
        methodsThatShouldExist_jdk6.put("digit", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk6.put("getNumericValue", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("getNumericValue", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isSpace", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isSpaceChar", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("isSpaceChar", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isWhitespace", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isWhitespace", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("isISOControl", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("isISOControl", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("forDigit", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk6.put("getDirectionality", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("getDirectionality", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isMirrored", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("isMirrored", new Class[] {int.class});

        Map<String, Class[]> methodsThatShouldExist_jdk7 = new HashMap<String, Class[]>();
        methodsThatShouldExist_jdk7.put("isJavaIdentifierStart", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isJavaIdentifierStart", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isJavaIdentifierPart", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isJavaIdentifierPart", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("equals", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk7.put("toString", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("toString", new Class[] {});
        methodsThatShouldExist_jdk7.put("hashCode", new Class[] {});
        methodsThatShouldExist_jdk7.put("reverseBytes", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("compareTo", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk7.put("compareTo", new Class[] {java.lang.Character.class});
        methodsThatShouldExist_jdk7.put("charValue", new Class[] {});
        methodsThatShouldExist_jdk7.put("valueOf", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("getName", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("charCount", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("codePointAt", new Class[] {(new char[1]).getClass(), int.class, int.class});
        methodsThatShouldExist_jdk7.put("codePointAt", new Class[] {java.lang.CharSequence.class, int.class});
        methodsThatShouldExist_jdk7.put("codePointAt", new Class[] {(new char[1]).getClass(), int.class});
        methodsThatShouldExist_jdk7.put("codePointAtImpl", new Class[] {(new char[1]).getClass(), int.class, int.class});
        methodsThatShouldExist_jdk7.put("codePointBefore", new Class[] {(new char[1]).getClass(), int.class, int.class});
        methodsThatShouldExist_jdk7.put("codePointBefore", new Class[] {(new char[1]).getClass(), int.class});
        methodsThatShouldExist_jdk7.put("codePointBefore", new Class[] {java.lang.CharSequence.class, int.class});
        methodsThatShouldExist_jdk7.put("codePointBeforeImpl", new Class[] {(new char[1]).getClass(), int.class, int.class});
        methodsThatShouldExist_jdk7.put("codePointCount", new Class[] {(new char[1]).getClass(), int.class, int.class});
        methodsThatShouldExist_jdk7.put("codePointCount", new Class[] {java.lang.CharSequence.class, int.class, int.class});
        methodsThatShouldExist_jdk7.put("codePointCountImpl", new Class[] {(new char[1]).getClass(), int.class, int.class});
        methodsThatShouldExist_jdk7.put("compare", new Class[] {char.class, char.class});
        methodsThatShouldExist_jdk7.put("highSurrogate", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isBmpCodePoint", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isValidCodePoint", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("lowSurrogate", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("offsetByCodePoints", new Class[] {(new char[1]).getClass(), int.class, int.class, int.class, int.class});
        methodsThatShouldExist_jdk7.put("offsetByCodePoints", new Class[] {java.lang.CharSequence.class, int.class, int.class});
        methodsThatShouldExist_jdk7.put("offsetByCodePointsImpl", new Class[] {(new char[1]).getClass(), int.class, int.class, int.class, int.class});
        methodsThatShouldExist_jdk7.put("toChars", new Class[] {int.class, (new char[1]).getClass(), int.class});
        methodsThatShouldExist_jdk7.put("toChars", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("toLowerCase", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("toLowerCase", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("toSurrogates", new Class[] {int.class, (new char[1]).getClass(), int.class});
        methodsThatShouldExist_jdk7.put("toUpperCase", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("toUpperCase", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("toUpperCaseCharArray", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("toUpperCaseEx", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("getType", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("getType", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isHighSurrogate", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isLowSurrogate", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("digit", new Class[] {char.class, int.class});
        methodsThatShouldExist_jdk7.put("digit", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk7.put("forDigit", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk7.put("getDirectionality", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("getDirectionality", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("getNumericValue", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("getNumericValue", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isAlphabetic", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isDefined", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isDefined", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isDigit", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isDigit", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isISOControl", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isISOControl", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isIdentifierIgnorable", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isIdentifierIgnorable", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isIdeographic", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isJavaLetter", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isJavaLetterOrDigit", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isLetter", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isLetter", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isLetterOrDigit", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isLetterOrDigit", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isLowerCase", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isLowerCase", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isMirrored", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isMirrored", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isSpace", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isSpaceChar", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isSpaceChar", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isSupplementaryCodePoint", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isSurrogate", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isSurrogatePair", new Class[] {char.class, char.class});
        methodsThatShouldExist_jdk7.put("isTitleCase", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isTitleCase", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isUnicodeIdentifierPart", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isUnicodeIdentifierPart", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isUnicodeIdentifierStart", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isUnicodeIdentifierStart", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isUpperCase", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isUpperCase", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("isWhitespace", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("isWhitespace", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("toCodePoint", new Class[] {char.class, char.class});
        methodsThatShouldExist_jdk7.put("toTitleCase", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("toTitleCase", new Class[] {int.class});

        // get the right map containing method signatures
        Map<String, Class[]> methodsThatShouldExist = getJavaVersion() < 7 ? methodsThatShouldExist_jdk6 : methodsThatShouldExist_jdk7;

        // create instance of a class Character
        final Object o = new Character('!');

        // get a runtime class of an object "o"
        final Class c = o.getClass();

        // check if all required methods really exist
        for (Map.Entry<String, Class[]> methodThatShouldExists : methodsThatShouldExist.entrySet()) {
            try {
                java.lang.reflect.Method method = c.getDeclaredMethod(methodThatShouldExists.getKey(), methodThatShouldExists.getValue());
                harness.check(method != null);
                String methodName = method.getName();
                harness.check(methodName != null);
                harness.check(methodName, methodThatShouldExists.getKey());
            }
            catch (Exception e) {
                harness.check(false);
            }
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

