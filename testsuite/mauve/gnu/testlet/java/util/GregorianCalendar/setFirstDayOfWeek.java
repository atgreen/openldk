// Tags: JDK1.1

// Copyright (C) 2006 Roman Kennke <kennke@aicas.com>

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


package gnu.testlet.java.util.GregorianCalendar;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Tests the setFirstDayOfWeek() method.
 * This testcase is derived from an actual bug in GregorianCalendar.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class setFirstDayOfWeek
  implements Testlet
{
  private int[] mondayDays = new int[]{27, 27, 27, 27, 27, 27, 6, 6, 6, 6, 6,
                                       6, 6, 13, 13, 13, 13, 13, 13, 13, 20,
                                       20, 20, 20, 20, 20, 20, 27, 27, 27, 27,
                                       27};
  private int[] mondayMonths = new int[]{1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2,
                                         2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                         2, 2, 2, 2, 2, 2, 2, 2, 2};

  /**
   * The entry point into the test.
   */
  public void test (TestHarness harness)
  {
    Locale.setDefault(Locale.GERMANY);
    Calendar calendar = Calendar.getInstance();
    calendar.setFirstDayOfWeek(Calendar.MONDAY);
    for (int day = 1; day <= 31; day++)
      {
        calendar.set(2006, Calendar.MARCH, day);
        // The day and month should be exactly as we set it.

        // Note: We need to query the WEEK_OF_YEAR, otherwise we don't get
        // the correct time in the last two tests in classpath.
        calendar.get(Calendar.WEEK_OF_YEAR);
        harness.check(calendar.get(Calendar.DAY_OF_MONTH), day);
        harness.check(calendar.get(Calendar.MONTH), Calendar.MARCH);

        calendar.set(Calendar.DAY_OF_WEEK, Calendar.MONDAY);
        // The day and month should now be according to the tables above.
        harness.check(calendar.get(Calendar.DAY_OF_MONTH), mondayDays[day]);
        harness.check(calendar.get(Calendar.MONTH), mondayMonths[day]);
      }
  }
}
