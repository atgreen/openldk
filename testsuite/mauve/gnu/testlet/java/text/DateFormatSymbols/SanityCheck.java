// Tags: JDK1.2

// Copyright (C) 2012 Andrew John Hughes (gnu_andrew@member.fsf.org)

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.text.DateFormatSymbols;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.text.DateFormatSymbols;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Locale;

/**
 * Checks that all locales return usable arrays of the
 * correct size.
 */
public class SanityCheck
  implements Testlet
{

  /**
   * Runs the test using the specified harness.
   *
   * @param harness the test harness.
   */
  public void test(TestHarness harness)
  {
    Locale[] locales = Locale.getAvailableLocales();
    for (int a = 0; a < locales.length; ++a)
      {
        DateFormatSymbols dfs = DateFormatSymbols.getInstance(locales[a]);
        checkArray(harness, locales[a], "AM/PM", dfs.getAmPmStrings(), 2);
        checkArray(harness, locales[a], "Eras", dfs.getEras(), 2);
        checkArray(harness, locales[a], "Months", dfs.getMonths(), 13);
        checkArray(harness, locales[a], "Short months", dfs.getShortMonths(), 13);
        checkArray(harness, locales[a], "Weekdays", dfs.getWeekdays(), 8);
        checkArray(harness, locales[a], "Short weekdays", dfs.getShortWeekdays(), 8);
      }
  }

  /**
   * Checks an array of locale data is of the correct size.
   *
   * @param harness the test harness.
   * @param locale the locale being tested (for display purposes).
   * @param type the type of data (for display purposes).
   * @param array the array of strings.
   * @param size the expected size of the array.
   */
  private void checkArray(TestHarness harness, Locale locale, String type,
                          String[] array, int size)
  {
    harness.check(array.length == size, type + "check (" + locale + ")");
    harness.debug(locale + ": " + type + "=" + Arrays.toString(array));
    // Check entries are non-null and also non-empty (unless allowed).
    for (int a = 0; a < array.length; ++a)
      {
        harness.check(array[a] != null, type + "[" + a + "] null check (" +
                      locale + ")");
        // Weekdays are indexed 1 to 7 not 0 to 6.  The 13th month is not
        // used by the Gregorian calendar.
        if (!(type.contains("onths") && a == Calendar.UNDECIMBER) &&
            !(type.contains("days") && a == 0))
          harness.check(!array[a].isEmpty(), type + "[" + a + "] empty check (" +
                        locale + ")");
      }
  }

}
