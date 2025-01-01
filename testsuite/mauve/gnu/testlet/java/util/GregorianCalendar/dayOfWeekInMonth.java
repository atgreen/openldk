// Copyright (C) 2007 Red Hat, Inc.
// Written by Gary Benson <gbenson@redhat.com>

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

// Tags: JDK1.1

package gnu.testlet.java.util.GregorianCalendar;

import java.util.Calendar;
import java.util.GregorianCalendar;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class dayOfWeekInMonth implements Testlet
{
  public void test(TestHarness harness)
  {
    GregorianCalendar c = new GregorianCalendar();
    GregorianCalendar d = new GregorianCalendar();
    GregorianCalendar e = new GregorianCalendar();

    // 31 day months whose first days are the specified weekdays
    int testMonths[][] =
      {{Calendar.JANUARY,  2007, Calendar.MONDAY},
       {Calendar.MAY,      2007, Calendar.TUESDAY},
       {Calendar.AUGUST,   2007, Calendar.WEDNESDAY},
       {Calendar.MARCH,    2007, Calendar.THURSDAY},
       {Calendar.DECEMBER, 2006, Calendar.FRIDAY},
       {Calendar.DECEMBER, 2007, Calendar.SATURDAY},
       {Calendar.JULY,     2007, Calendar.SUNDAY}};

    for (int minimalDaysInFirstWeek = 1;
	 minimalDaysInFirstWeek <= 7; minimalDaysInFirstWeek++) {

      c.setMinimalDaysInFirstWeek(minimalDaysInFirstWeek);
      d.setMinimalDaysInFirstWeek(minimalDaysInFirstWeek);
      e.setMinimalDaysInFirstWeek(minimalDaysInFirstWeek);

      for (int firstDayOfWeek = Calendar.SUNDAY;
	   firstDayOfWeek <= Calendar.SATURDAY; firstDayOfWeek++) {

	c.setFirstDayOfWeek(firstDayOfWeek);
	d.setFirstDayOfWeek(firstDayOfWeek);
	e.setFirstDayOfWeek(firstDayOfWeek);

	for (int i = 0; i < testMonths.length; i++) {
	  int month = testMonths[i][0];
	  int year  = testMonths[i][1];
	  int first = testMonths[i][2];

	  for (int day = 1; day <= 31; day++) {
	    // First we set YEAR + MONTH + DAY_OF_MONTH and check we
	    // have DAY_OF_WEEK_IN_MONTH and WEEK_OF_MONTH correct.

	    c.set(year, month, day);
	    int dayOfWeekInMonth = (day + 6) / 7;

	    harness.check(
	      c.get(Calendar.DAY_OF_WEEK_IN_MONTH) == dayOfWeekInMonth);

	    int dayOfWeek = c.get(Calendar.DAY_OF_WEEK);
	    if (day == 1)
	      harness.check(dayOfWeek == first); // sanity

	    // which day of the week are we (0..6) relative to firstDayOfWeek
	    int relativeDayOfWeek = (7 + dayOfWeek - firstDayOfWeek) % 7;

	    // which day of the week is the first of this month?
	    // nb 35 is the smallest multiple of 7 that ensures that
	    // the left hand side of the modulo operator is positive.
	    int relativeDayOfFirst = (relativeDayOfWeek - day + 1 + 35) % 7;

	    // which week of the month is the first of this month in?
	    int weekOfFirst =
	      ((7 - relativeDayOfFirst) >= minimalDaysInFirstWeek) ? 1 : 0;

	    // which week of the month is this day in?
	    int weekOfMonth =
	      (day + relativeDayOfFirst - 1) / 7 + weekOfFirst;

	    harness.check(c.get(Calendar.WEEK_OF_MONTH) == weekOfMonth);

	    // Then we set YEAR + MONTH + DAY_OF_WEEK_IN_MONTH +
	    // DAY_OF_WEEK and check we have DAY_OF_MONTH correct.

	    d.clear();
	    d.set(Calendar.YEAR, year);
	    d.set(Calendar.MONTH, month);
	    d.set(Calendar.DAY_OF_WEEK_IN_MONTH, dayOfWeekInMonth);
	    d.set(Calendar.DAY_OF_WEEK, dayOfWeek);
	    harness.check(d.get(Calendar.DAY_OF_MONTH) == day);

	    // Finally we set YEAR + MONTH + WEEK_OF_MONTH + DAY_OF_WEEK
	    // and check we have DAY_OF_MONTH correct.

	    e.clear();
	    e.set(Calendar.YEAR, year);
	    e.set(Calendar.MONTH, month);
	    e.set(Calendar.WEEK_OF_MONTH, weekOfMonth);
	    e.set(Calendar.DAY_OF_WEEK, dayOfWeek);
	    harness.check(e.get(Calendar.DAY_OF_MONTH) == day);
	  }
	}
      }
    }
  }
}
