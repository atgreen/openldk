// Tags: JDK1.4

// Copyright (C) 2004 Andrew John Hughes <gnu_andrew@member.fsf.org>

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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.text.NumberFormat;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Locale;

/**
 * Class to test <code>NumberFormat</code> for the UK.
 *
 * @author Andrew John Hughes <gnu_andrew@member.fsf.org>
 */
public class UK implements Testlet
{
  /* Locale-specific test data */
  private static final Locale TEST_LOCALE = Locale.UK; 
  private static final String EXPECTED_GROUPED_NUMBER = "123,456.789"; 
  private static final String EXPECTED_INT_GROUP_NUMBER = "123,457"; 
  private static final String EXPECTED_PER_GROUP_NUMBER = "12,345,678.9%"; 
  private static final String CURRENCY_SYMBOL = "\u00A3"; 
  private static final boolean CURRENCY_PREFIXED = true; 
  private static final String DECIMAL_SEP = "."; 
  private static final String CURRENCY_SUFFIX = DECIMAL_SEP + "00"; 
  private static final String GROUPED_PERCENTILE = "3,000%";

  public void test(TestHarness harness)
  {
    NumberFormat numberFormat;
    double testDouble;
    long testLong;
    String testString;

    /********************************** NORMAL NUMBERS ****************************************/

    /* Get an instance for normal numbers in the test locale */
    numberFormat = NumberFormat.getNumberInstance(TEST_LOCALE);
    /* Set the options on the number formatter */
    setOptions(numberFormat, false);
    /* Format an long-based integer using the normal format */
    testLong = 30;
    testString = numberFormat.format(testLong);
    harness.check(testString, "30", "Long-based integer formatting with normal number format ("+
                  testString + ").");
    /* Format an double-based integer using the normal format */
    testDouble = 30;
    testString = numberFormat.format(testDouble);
    harness.check(testString, "30", "Double-based integer formatting with normal number format ("+
                  testString + ").");
    /* Format an double-based fraction using the normal format */
    testDouble = 1.0 / 3;
    testString = numberFormat.format(testDouble);
    harness.check(testString, "0.333", "Double-based fraction formatting with normal number format ("+
                  testString + ").");
    /* Format an double-based decimal number using the normal format */
    testDouble = 123456.789;
    testString = numberFormat.format(testDouble);
    harness.check(testString, EXPECTED_GROUPED_NUMBER, "Double-based fraction formatting with normal number format ("+
                  testString + ").");

    /********************************** INTEGER NUMBERS ****************************************/

    /* Get an instance for integer numbers in the test locale */
    numberFormat = NumberFormat.getIntegerInstance(TEST_LOCALE);
    /* Set the options on the number formatter */
    setOptions(numberFormat, true);
    /* Format an long-based integer using the integer format */
    testLong = 30;
    testString = numberFormat.format(testLong);
    harness.check(testString, "30", "Long-based integer formatting with integer number format ("+
                  testString + ").");
    /* Format an double-based integer using the integer format */
    testDouble = 30;
    testString = numberFormat.format(testDouble);
    harness.check(testString, "30", "Double-based integer formatting with integer number format ("+
                  testString + ").");
    /* Format an double-based fraction using the integer format */
    testDouble = 1.0 / 3;
    testString = numberFormat.format(testDouble);
    harness.check(testString, "0", "Double-based fraction formatting with integer number format ("+
                  testString + ").");
    /* Format an double-based decimal number using the integer format */
    testDouble = 123456.789;
    testString = numberFormat.format(testDouble);
    harness.check(testString, EXPECTED_INT_GROUP_NUMBER, "Double-based fraction formatting with integer number format ("+
                  testString + ").");

    /********************************** CURRENCIES ****************************************/

    /* Get an instance for currency numbers in the test locale */
    numberFormat = NumberFormat.getCurrencyInstance(TEST_LOCALE);
    /* Set the options on the number formatter */
    setOptions(numberFormat, false);
    /* Format an long-based integer using the currency format */
    testLong = 30;
    testString = numberFormat.format(testLong);
    if (CURRENCY_PREFIXED)
      {
        harness.check(testString, CURRENCY_SYMBOL + "30" + CURRENCY_SUFFIX,
                      "Long-based integer formatting with currency number format ("+
                      testString + ").");
      }
    else
      {
        harness.check(testString, "30" + CURRENCY_SUFFIX + CURRENCY_SYMBOL,
                      "Long-based integer formatting with currency number format ("+
                      testString + ").");
      }
    /* Format an double-based integer using the currency format */
    testDouble = 30;
    testString = numberFormat.format(testDouble);
    if (CURRENCY_PREFIXED)
      {
        harness.check(testString, CURRENCY_SYMBOL + "30" + CURRENCY_SUFFIX,
                      "Double-based integer formatting with currency number format ("+
                      testString + ").");
      } 
    else
      {
        harness.check(testString, "30" + CURRENCY_SUFFIX + CURRENCY_SYMBOL,
                      "Double-based integer formatting with currency number format ("+
                      testString + ").");
      }  
    /* Format an double-based fraction using the currency format */
    testDouble = 1.0 / 3;
    testString = numberFormat.format(testDouble);
    if (CURRENCY_PREFIXED)
      {
        harness.check(testString, CURRENCY_SYMBOL + "0.333", "Double-based fraction formatting with currency number format ("+
                      testString + ").");
      }
    else 
      {
        harness.check(testString, "0.333" + CURRENCY_SYMBOL, "Double-based fraction formatting with currency number format ("+
                      testString + ").");
      }
    /* Format an double-based decimal number using the currency format */
    testDouble = 123456.789;
    testString = numberFormat.format(testDouble);
    if (CURRENCY_PREFIXED)
      {
        harness.check(testString, CURRENCY_SYMBOL + EXPECTED_GROUPED_NUMBER,
                      "Double-based fraction formatting with currency number format ("+
                      testString + ").");
      }
    else
      {
        harness.check(testString, EXPECTED_GROUPED_NUMBER + CURRENCY_SYMBOL,
                      "Double-based fraction formatting with currency number format ("+
                      testString + ").");
      }

    /********************************** PERCENTILES ****************************************/

    /* Get an instance for percentile numbers in the test locale */
    numberFormat = NumberFormat.getPercentInstance(TEST_LOCALE);
    /* Set the options on the number formatter */
    setOptions(numberFormat, false);
    /* Format an long-based integer using the percentile format */
    testLong = 30;
    testString = numberFormat.format(testLong);
    harness.check(testString, GROUPED_PERCENTILE, "Long-based integer formatting with percentile number format ("+
                  testString + ").");
    /* Format an double-based integer using the percentile format */
    testDouble = 30;
    testString = numberFormat.format(testDouble);
    harness.check(testString, GROUPED_PERCENTILE, "Double-based integer formatting with percentile number format ("+
                  testString + ").");
    /* Format an double-based fraction using the percentile format */
    testDouble = 1.0 / 3;
    testString = numberFormat.format(testDouble);
    harness.check(testString, "33.333%", "Double-based fraction formatting with percentile number format ("+
                  testString + ").");
    /* Format an double-based decimal number using the percentile format */
    testDouble = 123456.789;
    testString = numberFormat.format(testDouble);
    harness.check(testString, EXPECTED_PER_GROUP_NUMBER, "Double-based fraction formatting with percentile number format ("+
                  testString + ").");

  }

  /**
   * Sets the options for the formatting of numbers to receive expected output.
   * The options themselves are tested elsewhere.
   *
   * @param format the number formatter to set up.
   * @param integer true if integer formatting is being used.
   */
  public void setOptions(NumberFormat formatter, boolean integer)
  {
    if (!integer)
      {
	formatter.setMaximumFractionDigits(3); /* Stop at 3 digits after the decimal point */
      }
    formatter.setGroupingUsed(true); /* Turn on grouping */
    try
      {
        ((DecimalFormat) formatter).setDecimalSeparatorAlwaysShown(false); /* Don't always show the decimal separator */
      }
    catch (ClassCastException exception)
      {
        /* Formatter is not an instance of the DecimalFormat subclass */
      }
  }

}
