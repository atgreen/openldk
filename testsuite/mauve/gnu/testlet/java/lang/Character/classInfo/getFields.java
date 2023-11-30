// Test for method java.lang.Character.getClass().getFields()

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
 * Test for method java.lang.Character.getClass().getFields()
 */
public class getFields implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // map of fields which should exists
        Map<String, String> testedFields = null;

        // map of fields for (Open)JDK6
        Map<String, String> testedFields_jdk6 = new HashMap<String, String>();

        // map of fields for (Open)JDK7
        Map<String, String> testedFields_jdk7 = new HashMap<String, String>();

        // map for fields declared in (Open)JDK6
        testedFields_jdk6.put("public static final int java.lang.Character.MIN_RADIX", "MIN_RADIX");
        testedFields_jdk6.put("public static final int java.lang.Character.MAX_RADIX", "MAX_RADIX");
        testedFields_jdk6.put("public static final char java.lang.Character.MIN_VALUE", "MIN_VALUE");
        testedFields_jdk6.put("public static final char java.lang.Character.MAX_VALUE", "MAX_VALUE");
        testedFields_jdk6.put("public static final java.lang.Class java.lang.Character.TYPE", "TYPE");
        testedFields_jdk6.put("public static final byte java.lang.Character.UNASSIGNED", "UNASSIGNED");
        testedFields_jdk6.put("public static final byte java.lang.Character.UPPERCASE_LETTER", "UPPERCASE_LETTER");
        testedFields_jdk6.put("public static final byte java.lang.Character.LOWERCASE_LETTER", "LOWERCASE_LETTER");
        testedFields_jdk6.put("public static final byte java.lang.Character.TITLECASE_LETTER", "TITLECASE_LETTER");
        testedFields_jdk6.put("public static final byte java.lang.Character.MODIFIER_LETTER", "MODIFIER_LETTER");
        testedFields_jdk6.put("public static final byte java.lang.Character.OTHER_LETTER", "OTHER_LETTER");
        testedFields_jdk6.put("public static final byte java.lang.Character.NON_SPACING_MARK", "NON_SPACING_MARK");
        testedFields_jdk6.put("public static final byte java.lang.Character.ENCLOSING_MARK", "ENCLOSING_MARK");
        testedFields_jdk6.put("public static final byte java.lang.Character.COMBINING_SPACING_MARK", "COMBINING_SPACING_MARK");
        testedFields_jdk6.put("public static final byte java.lang.Character.DECIMAL_DIGIT_NUMBER", "DECIMAL_DIGIT_NUMBER");
        testedFields_jdk6.put("public static final byte java.lang.Character.LETTER_NUMBER", "LETTER_NUMBER");
        testedFields_jdk6.put("public static final byte java.lang.Character.OTHER_NUMBER", "OTHER_NUMBER");
        testedFields_jdk6.put("public static final byte java.lang.Character.SPACE_SEPARATOR", "SPACE_SEPARATOR");
        testedFields_jdk6.put("public static final byte java.lang.Character.LINE_SEPARATOR", "LINE_SEPARATOR");
        testedFields_jdk6.put("public static final byte java.lang.Character.PARAGRAPH_SEPARATOR", "PARAGRAPH_SEPARATOR");
        testedFields_jdk6.put("public static final byte java.lang.Character.CONTROL", "CONTROL");
        testedFields_jdk6.put("public static final byte java.lang.Character.FORMAT", "FORMAT");
        testedFields_jdk6.put("public static final byte java.lang.Character.PRIVATE_USE", "PRIVATE_USE");
        testedFields_jdk6.put("public static final byte java.lang.Character.SURROGATE", "SURROGATE");
        testedFields_jdk6.put("public static final byte java.lang.Character.DASH_PUNCTUATION", "DASH_PUNCTUATION");
        testedFields_jdk6.put("public static final byte java.lang.Character.START_PUNCTUATION", "START_PUNCTUATION");
        testedFields_jdk6.put("public static final byte java.lang.Character.END_PUNCTUATION", "END_PUNCTUATION");
        testedFields_jdk6.put("public static final byte java.lang.Character.CONNECTOR_PUNCTUATION", "CONNECTOR_PUNCTUATION");
        testedFields_jdk6.put("public static final byte java.lang.Character.OTHER_PUNCTUATION", "OTHER_PUNCTUATION");
        testedFields_jdk6.put("public static final byte java.lang.Character.MATH_SYMBOL", "MATH_SYMBOL");
        testedFields_jdk6.put("public static final byte java.lang.Character.CURRENCY_SYMBOL", "CURRENCY_SYMBOL");
        testedFields_jdk6.put("public static final byte java.lang.Character.MODIFIER_SYMBOL", "MODIFIER_SYMBOL");
        testedFields_jdk6.put("public static final byte java.lang.Character.OTHER_SYMBOL", "OTHER_SYMBOL");
        testedFields_jdk6.put("public static final byte java.lang.Character.INITIAL_QUOTE_PUNCTUATION", "INITIAL_QUOTE_PUNCTUATION");
        testedFields_jdk6.put("public static final byte java.lang.Character.FINAL_QUOTE_PUNCTUATION", "FINAL_QUOTE_PUNCTUATION");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_UNDEFINED", "DIRECTIONALITY_UNDEFINED");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_LEFT_TO_RIGHT", "DIRECTIONALITY_LEFT_TO_RIGHT");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_RIGHT_TO_LEFT", "DIRECTIONALITY_RIGHT_TO_LEFT");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC", "DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_EUROPEAN_NUMBER", "DIRECTIONALITY_EUROPEAN_NUMBER");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_EUROPEAN_NUMBER_SEPARATOR", "DIRECTIONALITY_EUROPEAN_NUMBER_SEPARATOR");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR", "DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_ARABIC_NUMBER", "DIRECTIONALITY_ARABIC_NUMBER");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_COMMON_NUMBER_SEPARATOR", "DIRECTIONALITY_COMMON_NUMBER_SEPARATOR");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_NONSPACING_MARK", "DIRECTIONALITY_NONSPACING_MARK");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_BOUNDARY_NEUTRAL", "DIRECTIONALITY_BOUNDARY_NEUTRAL");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_PARAGRAPH_SEPARATOR", "DIRECTIONALITY_PARAGRAPH_SEPARATOR");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_SEGMENT_SEPARATOR", "DIRECTIONALITY_SEGMENT_SEPARATOR");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_WHITESPACE", "DIRECTIONALITY_WHITESPACE");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_OTHER_NEUTRALS", "DIRECTIONALITY_OTHER_NEUTRALS");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_LEFT_TO_RIGHT_EMBEDDING", "DIRECTIONALITY_LEFT_TO_RIGHT_EMBEDDING");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_LEFT_TO_RIGHT_OVERRIDE", "DIRECTIONALITY_LEFT_TO_RIGHT_OVERRIDE");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_RIGHT_TO_LEFT_EMBEDDING", "DIRECTIONALITY_RIGHT_TO_LEFT_EMBEDDING");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_RIGHT_TO_LEFT_OVERRIDE", "DIRECTIONALITY_RIGHT_TO_LEFT_OVERRIDE");
        testedFields_jdk6.put("public static final byte java.lang.Character.DIRECTIONALITY_POP_DIRECTIONAL_FORMAT", "DIRECTIONALITY_POP_DIRECTIONAL_FORMAT");
        testedFields_jdk6.put("public static final char java.lang.Character.MIN_HIGH_SURROGATE", "MIN_HIGH_SURROGATE");
        testedFields_jdk6.put("public static final char java.lang.Character.MAX_HIGH_SURROGATE", "MAX_HIGH_SURROGATE");
        testedFields_jdk6.put("public static final char java.lang.Character.MIN_LOW_SURROGATE", "MIN_LOW_SURROGATE");
        testedFields_jdk6.put("public static final char java.lang.Character.MAX_LOW_SURROGATE", "MAX_LOW_SURROGATE");
        testedFields_jdk6.put("public static final char java.lang.Character.MIN_SURROGATE", "MIN_SURROGATE");
        testedFields_jdk6.put("public static final char java.lang.Character.MAX_SURROGATE", "MAX_SURROGATE");
        testedFields_jdk6.put("public static final int java.lang.Character.MIN_SUPPLEMENTARY_CODE_POINT", "MIN_SUPPLEMENTARY_CODE_POINT");
        testedFields_jdk6.put("public static final int java.lang.Character.MIN_CODE_POINT", "MIN_CODE_POINT");
        testedFields_jdk6.put("public static final int java.lang.Character.MAX_CODE_POINT", "MAX_CODE_POINT");
        testedFields_jdk6.put("public static final int java.lang.Character.SIZE", "SIZE");

        // map for fields declared in (Open)JDK7
        testedFields_jdk7.put("public static final int java.lang.Character.MIN_RADIX", "MIN_RADIX");
        testedFields_jdk7.put("public static final int java.lang.Character.MAX_RADIX", "MAX_RADIX");
        testedFields_jdk7.put("public static final char java.lang.Character.MIN_VALUE", "MIN_VALUE");
        testedFields_jdk7.put("public static final char java.lang.Character.MAX_VALUE", "MAX_VALUE");
        testedFields_jdk7.put("public static final java.lang.Class java.lang.Character.TYPE", "TYPE");
        testedFields_jdk7.put("public static final byte java.lang.Character.UNASSIGNED", "UNASSIGNED");
        testedFields_jdk7.put("public static final byte java.lang.Character.UPPERCASE_LETTER", "UPPERCASE_LETTER");
        testedFields_jdk7.put("public static final byte java.lang.Character.LOWERCASE_LETTER", "LOWERCASE_LETTER");
        testedFields_jdk7.put("public static final byte java.lang.Character.TITLECASE_LETTER", "TITLECASE_LETTER");
        testedFields_jdk7.put("public static final byte java.lang.Character.MODIFIER_LETTER", "MODIFIER_LETTER");
        testedFields_jdk7.put("public static final byte java.lang.Character.OTHER_LETTER", "OTHER_LETTER");
        testedFields_jdk7.put("public static final byte java.lang.Character.NON_SPACING_MARK", "NON_SPACING_MARK");
        testedFields_jdk7.put("public static final byte java.lang.Character.ENCLOSING_MARK", "ENCLOSING_MARK");
        testedFields_jdk7.put("public static final byte java.lang.Character.COMBINING_SPACING_MARK", "COMBINING_SPACING_MARK");
        testedFields_jdk7.put("public static final byte java.lang.Character.DECIMAL_DIGIT_NUMBER", "DECIMAL_DIGIT_NUMBER");
        testedFields_jdk7.put("public static final byte java.lang.Character.LETTER_NUMBER", "LETTER_NUMBER");
        testedFields_jdk7.put("public static final byte java.lang.Character.OTHER_NUMBER", "OTHER_NUMBER");
        testedFields_jdk7.put("public static final byte java.lang.Character.SPACE_SEPARATOR", "SPACE_SEPARATOR");
        testedFields_jdk7.put("public static final byte java.lang.Character.LINE_SEPARATOR", "LINE_SEPARATOR");
        testedFields_jdk7.put("public static final byte java.lang.Character.PARAGRAPH_SEPARATOR", "PARAGRAPH_SEPARATOR");
        testedFields_jdk7.put("public static final byte java.lang.Character.CONTROL", "CONTROL");
        testedFields_jdk7.put("public static final byte java.lang.Character.FORMAT", "FORMAT");
        testedFields_jdk7.put("public static final byte java.lang.Character.PRIVATE_USE", "PRIVATE_USE");
        testedFields_jdk7.put("public static final byte java.lang.Character.SURROGATE", "SURROGATE");
        testedFields_jdk7.put("public static final byte java.lang.Character.DASH_PUNCTUATION", "DASH_PUNCTUATION");
        testedFields_jdk7.put("public static final byte java.lang.Character.START_PUNCTUATION", "START_PUNCTUATION");
        testedFields_jdk7.put("public static final byte java.lang.Character.END_PUNCTUATION", "END_PUNCTUATION");
        testedFields_jdk7.put("public static final byte java.lang.Character.CONNECTOR_PUNCTUATION", "CONNECTOR_PUNCTUATION");
        testedFields_jdk7.put("public static final byte java.lang.Character.OTHER_PUNCTUATION", "OTHER_PUNCTUATION");
        testedFields_jdk7.put("public static final byte java.lang.Character.MATH_SYMBOL", "MATH_SYMBOL");
        testedFields_jdk7.put("public static final byte java.lang.Character.CURRENCY_SYMBOL", "CURRENCY_SYMBOL");
        testedFields_jdk7.put("public static final byte java.lang.Character.MODIFIER_SYMBOL", "MODIFIER_SYMBOL");
        testedFields_jdk7.put("public static final byte java.lang.Character.OTHER_SYMBOL", "OTHER_SYMBOL");
        testedFields_jdk7.put("public static final byte java.lang.Character.INITIAL_QUOTE_PUNCTUATION", "INITIAL_QUOTE_PUNCTUATION");
        testedFields_jdk7.put("public static final byte java.lang.Character.FINAL_QUOTE_PUNCTUATION", "FINAL_QUOTE_PUNCTUATION");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_UNDEFINED", "DIRECTIONALITY_UNDEFINED");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_LEFT_TO_RIGHT", "DIRECTIONALITY_LEFT_TO_RIGHT");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_RIGHT_TO_LEFT", "DIRECTIONALITY_RIGHT_TO_LEFT");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC", "DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_EUROPEAN_NUMBER", "DIRECTIONALITY_EUROPEAN_NUMBER");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_EUROPEAN_NUMBER_SEPARATOR", "DIRECTIONALITY_EUROPEAN_NUMBER_SEPARATOR");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR", "DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_ARABIC_NUMBER", "DIRECTIONALITY_ARABIC_NUMBER");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_COMMON_NUMBER_SEPARATOR", "DIRECTIONALITY_COMMON_NUMBER_SEPARATOR");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_NONSPACING_MARK", "DIRECTIONALITY_NONSPACING_MARK");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_BOUNDARY_NEUTRAL", "DIRECTIONALITY_BOUNDARY_NEUTRAL");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_PARAGRAPH_SEPARATOR", "DIRECTIONALITY_PARAGRAPH_SEPARATOR");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_SEGMENT_SEPARATOR", "DIRECTIONALITY_SEGMENT_SEPARATOR");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_WHITESPACE", "DIRECTIONALITY_WHITESPACE");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_OTHER_NEUTRALS", "DIRECTIONALITY_OTHER_NEUTRALS");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_LEFT_TO_RIGHT_EMBEDDING", "DIRECTIONALITY_LEFT_TO_RIGHT_EMBEDDING");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_LEFT_TO_RIGHT_OVERRIDE", "DIRECTIONALITY_LEFT_TO_RIGHT_OVERRIDE");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_RIGHT_TO_LEFT_EMBEDDING", "DIRECTIONALITY_RIGHT_TO_LEFT_EMBEDDING");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_RIGHT_TO_LEFT_OVERRIDE", "DIRECTIONALITY_RIGHT_TO_LEFT_OVERRIDE");
        testedFields_jdk7.put("public static final byte java.lang.Character.DIRECTIONALITY_POP_DIRECTIONAL_FORMAT", "DIRECTIONALITY_POP_DIRECTIONAL_FORMAT");
        testedFields_jdk7.put("public static final char java.lang.Character.MIN_HIGH_SURROGATE", "MIN_HIGH_SURROGATE");
        testedFields_jdk7.put("public static final char java.lang.Character.MAX_HIGH_SURROGATE", "MAX_HIGH_SURROGATE");
        testedFields_jdk7.put("public static final char java.lang.Character.MIN_LOW_SURROGATE", "MIN_LOW_SURROGATE");
        testedFields_jdk7.put("public static final char java.lang.Character.MAX_LOW_SURROGATE", "MAX_LOW_SURROGATE");
        testedFields_jdk7.put("public static final char java.lang.Character.MIN_SURROGATE", "MIN_SURROGATE");
        testedFields_jdk7.put("public static final char java.lang.Character.MAX_SURROGATE", "MAX_SURROGATE");
        testedFields_jdk7.put("public static final int java.lang.Character.MIN_SUPPLEMENTARY_CODE_POINT", "MIN_SUPPLEMENTARY_CODE_POINT");
        testedFields_jdk7.put("public static final int java.lang.Character.MIN_CODE_POINT", "MIN_CODE_POINT");
        testedFields_jdk7.put("public static final int java.lang.Character.MAX_CODE_POINT", "MAX_CODE_POINT");
        testedFields_jdk7.put("public static final int java.lang.Character.SIZE", "SIZE");

        // create instance of a class Character
        final Object o = new Character('!');

        // get a runtime class of an object "o"
        final Class c = o.getClass();

        // get the right map containing field signatures
        testedFields = getJavaVersion() < 7 ? testedFields_jdk6 : testedFields_jdk7;

        // get all fields for this class
        java.lang.reflect.Field[] fields = c.getFields();

        // expected number of fields
        final int expectedNumberOfFields = testedFields.size();

        // basic check for a number of fields
        harness.check(fields.length, expectedNumberOfFields);

        // check if all fields exist
        for (java.lang.reflect.Field field: fields) {
            String fieldName = field.getName();
            String fieldString = field.toString();
            harness.check(testedFields.containsKey(fieldString));
            harness.check(testedFields.get(fieldString), fieldName);
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

