// Test for method java.lang.Character.getClass().getDeclaredField()

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
 * Test for method java.lang.Character.getClass().getDeclaredField()
 */
public class getDeclaredField implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // following declared fields should exists
        final String[] fieldsThatShouldExist_jdk6 = {
            "MIN_RADIX",
            "MAX_RADIX",
            "MIN_VALUE",
            "MAX_VALUE",
            "TYPE",
            "UNASSIGNED",
            "UPPERCASE_LETTER",
            "LOWERCASE_LETTER",
            "TITLECASE_LETTER",
            "MODIFIER_LETTER",
            "OTHER_LETTER",
            "NON_SPACING_MARK",
            "ENCLOSING_MARK",
            "COMBINING_SPACING_MARK",
            "DECIMAL_DIGIT_NUMBER",
            "LETTER_NUMBER",
            "OTHER_NUMBER",
            "SPACE_SEPARATOR",
            "LINE_SEPARATOR",
            "PARAGRAPH_SEPARATOR",
            "CONTROL",
            "FORMAT",
            "PRIVATE_USE",
            "SURROGATE",
            "DASH_PUNCTUATION",
            "START_PUNCTUATION",
            "END_PUNCTUATION",
            "CONNECTOR_PUNCTUATION",
            "OTHER_PUNCTUATION",
            "MATH_SYMBOL",
            "CURRENCY_SYMBOL",
            "MODIFIER_SYMBOL",
            "OTHER_SYMBOL",
            "INITIAL_QUOTE_PUNCTUATION",
            "FINAL_QUOTE_PUNCTUATION",
            "ERROR",
            "DIRECTIONALITY_UNDEFINED",
            "DIRECTIONALITY_LEFT_TO_RIGHT",
            "DIRECTIONALITY_RIGHT_TO_LEFT",
            "DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC",
            "DIRECTIONALITY_EUROPEAN_NUMBER",
            "DIRECTIONALITY_EUROPEAN_NUMBER_SEPARATOR",
            "DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR",
            "DIRECTIONALITY_ARABIC_NUMBER",
            "DIRECTIONALITY_COMMON_NUMBER_SEPARATOR",
            "DIRECTIONALITY_NONSPACING_MARK",
            "DIRECTIONALITY_BOUNDARY_NEUTRAL",
            "DIRECTIONALITY_PARAGRAPH_SEPARATOR",
            "DIRECTIONALITY_SEGMENT_SEPARATOR",
            "DIRECTIONALITY_WHITESPACE",
            "DIRECTIONALITY_OTHER_NEUTRALS",
            "DIRECTIONALITY_LEFT_TO_RIGHT_EMBEDDING",
            "DIRECTIONALITY_LEFT_TO_RIGHT_OVERRIDE",
            "DIRECTIONALITY_RIGHT_TO_LEFT_EMBEDDING",
            "DIRECTIONALITY_RIGHT_TO_LEFT_OVERRIDE",
            "DIRECTIONALITY_POP_DIRECTIONAL_FORMAT",
            "MIN_HIGH_SURROGATE",
            "MAX_HIGH_SURROGATE",
            "MIN_LOW_SURROGATE",
            "MAX_LOW_SURROGATE",
            "MIN_SURROGATE",
            "MAX_SURROGATE",
            "MIN_SUPPLEMENTARY_CODE_POINT",
            "MIN_CODE_POINT",
            "MAX_CODE_POINT",
            "value",
            "serialVersionUID",
            "SIZE",
            "$assertionsDisabled",
        };
        final String[] fieldsThatShouldExist_jdk7 = {
            "MIN_RADIX",
            "MAX_RADIX",
            "MIN_VALUE",
            "MAX_VALUE",
            "TYPE",
            "UNASSIGNED",
            "UPPERCASE_LETTER",
            "LOWERCASE_LETTER",
            "TITLECASE_LETTER",
            "MODIFIER_LETTER",
            "OTHER_LETTER",
            "NON_SPACING_MARK",
            "ENCLOSING_MARK",
            "COMBINING_SPACING_MARK",
            "DECIMAL_DIGIT_NUMBER",
            "LETTER_NUMBER",
            "OTHER_NUMBER",
            "SPACE_SEPARATOR",
            "LINE_SEPARATOR",
            "PARAGRAPH_SEPARATOR",
            "CONTROL",
            "FORMAT",
            "PRIVATE_USE",
            "SURROGATE",
            "DASH_PUNCTUATION",
            "START_PUNCTUATION",
            "END_PUNCTUATION",
            "CONNECTOR_PUNCTUATION",
            "OTHER_PUNCTUATION",
            "MATH_SYMBOL",
            "CURRENCY_SYMBOL",
            "MODIFIER_SYMBOL",
            "OTHER_SYMBOL",
            "INITIAL_QUOTE_PUNCTUATION",
            "FINAL_QUOTE_PUNCTUATION",
            "ERROR",
            "DIRECTIONALITY_UNDEFINED",
            "DIRECTIONALITY_LEFT_TO_RIGHT",
            "DIRECTIONALITY_RIGHT_TO_LEFT",
            "DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC",
            "DIRECTIONALITY_EUROPEAN_NUMBER",
            "DIRECTIONALITY_EUROPEAN_NUMBER_SEPARATOR",
            "DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR",
            "DIRECTIONALITY_ARABIC_NUMBER",
            "DIRECTIONALITY_COMMON_NUMBER_SEPARATOR",
            "DIRECTIONALITY_NONSPACING_MARK",
            "DIRECTIONALITY_BOUNDARY_NEUTRAL",
            "DIRECTIONALITY_PARAGRAPH_SEPARATOR",
            "DIRECTIONALITY_SEGMENT_SEPARATOR",
            "DIRECTIONALITY_WHITESPACE",
            "DIRECTIONALITY_OTHER_NEUTRALS",
            "DIRECTIONALITY_LEFT_TO_RIGHT_EMBEDDING",
            "DIRECTIONALITY_LEFT_TO_RIGHT_OVERRIDE",
            "DIRECTIONALITY_RIGHT_TO_LEFT_EMBEDDING",
            "DIRECTIONALITY_RIGHT_TO_LEFT_OVERRIDE",
            "DIRECTIONALITY_POP_DIRECTIONAL_FORMAT",
            "MIN_HIGH_SURROGATE",
            "MAX_HIGH_SURROGATE",
            "MIN_LOW_SURROGATE",
            "MAX_LOW_SURROGATE",
            "MIN_SURROGATE",
            "MAX_SURROGATE",
            "MIN_SUPPLEMENTARY_CODE_POINT",
            "MIN_CODE_POINT",
            "MAX_CODE_POINT",
            "value",
            "serialVersionUID",
            "SIZE",
            "$assertionsDisabled",
        };

        final String[] fieldsThatShouldExist = getJavaVersion() < 7 ? fieldsThatShouldExist_jdk6 : fieldsThatShouldExist_jdk7;

        // create instance of a class Character
        final Object o = new Character('!');

        // get a runtime class of an object "o"
        final Class c = o.getClass();

        // check if all required fields really exists
        for (String fieldThatShouldExists : fieldsThatShouldExist) {
            try {
                java.lang.reflect.Field field = c.getDeclaredField(fieldThatShouldExists);
                harness.check(field != null);
                String fieldName = field.getName();
                harness.check(fieldName != null);
                harness.check(fieldName, fieldThatShouldExists);
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

