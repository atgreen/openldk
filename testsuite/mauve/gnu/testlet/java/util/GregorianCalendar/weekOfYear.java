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

public class weekOfYear implements Testlet
{
  public void test(TestHarness harness)
  {
    GregorianCalendar d = new GregorianCalendar();
    GregorianCalendar e = new GregorianCalendar();

    // Years whose first days are the specified weekdays
    int testYears[][] =
      {{2007, 1996, Calendar.MONDAY},
       {2002, 2008, Calendar.TUESDAY},
       {2003, 1992, Calendar.WEDNESDAY},
       {1998, 2004, Calendar.THURSDAY},
       {1999, 1988, Calendar.FRIDAY},
       {2005, 2000, Calendar.SATURDAY},
       {2006, 1956, Calendar.SUNDAY}};

    // Months to test.  We definitely want January here,
    // to check the calculation where January 1 falls
    // before week 1 of the year.
    int testMonths[] = {Calendar.JANUARY, Calendar.JULY};

    int monthLengths[] = {31, 28, 31, 30, 31, 30,
			  31, 31, 30, 31, 30, 31};
      
    for (int minimalDaysInFirstWeek = 1;
	 minimalDaysInFirstWeek <= 7; minimalDaysInFirstWeek++) {

      d.setMinimalDaysInFirstWeek(minimalDaysInFirstWeek);
      e.setMinimalDaysInFirstWeek(minimalDaysInFirstWeek);

      for (int firstDayOfWeek = Calendar.SUNDAY;
	   firstDayOfWeek <= Calendar.SATURDAY; firstDayOfWeek++) {

	d.setFirstDayOfWeek(firstDayOfWeek);
	e.setFirstDayOfWeek(firstDayOfWeek);

	for (int i = 0; i < testYears.length; i++) {
	  int dayOfFirst = testYears[i][2];

	  for (int leap = 0; leap <= 1; leap++) {
	    int year  = testYears[i][leap];

	    for (int j = 0; j < testMonths.length; j++) {
	      int month = testMonths[j];
	    
	      for (int day = 1; day <= 31; day++) {

		// First we set YEAR + MONTH + DAY_OF_MONTH and check
		// we have DAY_OF_YEAR and WEEK_OF_YEAR correct.

		GregorianCalendar c = new GregorianCalendar(year, month, day);
		c.setMinimalDaysInFirstWeek(minimalDaysInFirstWeek);
		c.setFirstDayOfWeek(firstDayOfWeek);
      
		// Which day of the year is this?
		int dayOfYear = 0;
		for (int k = 0; k < month; k++)
		  dayOfYear += monthLengths[k];
		if (month > Calendar.FEBRUARY)
		  dayOfYear += leap;
		dayOfYear += day;

		harness.check(c.get(Calendar.DAY_OF_YEAR) == dayOfYear);

		// Which day of the week is it?
		int dayOfWeek = (dayOfFirst + dayOfYear - 2) % 7 + 1;
		harness.check(c.get(Calendar.DAY_OF_WEEK) == dayOfWeek);

		// Which day of the week is the first of the year,
		// relative to firstDayOfWeek.
		int relativeDayOfFirst = (7 + dayOfFirst - firstDayOfWeek) % 7;

		// Which day of the year is week one, day one?
		int dayOfYearOfWeek1Day1 = 1 - relativeDayOfFirst;
		if ((7 - relativeDayOfFirst) < minimalDaysInFirstWeek)
		  dayOfYearOfWeek1Day1 += 7;

		// If we're before week one, day one then the week
		// number will be with respect to the previous year.
		// I think this is crack, but there you go.
		int checkYear = year;
		if (dayOfYear < dayOfYearOfWeek1Day1)
		  {
		    checkYear--;
		    if (checkYear % 4 == 0)
		      {
			relativeDayOfFirst = (relativeDayOfFirst + 5) % 7;
			dayOfYear += 366;
		      }
		    else
		      {
			relativeDayOfFirst = (relativeDayOfFirst + 6) % 7;
			dayOfYear += 365;
		      }

		    dayOfYearOfWeek1Day1 = 1 - relativeDayOfFirst;
		    if ((7 - relativeDayOfFirst) < minimalDaysInFirstWeek)
		      dayOfYearOfWeek1Day1 += 7;
		  }

		// Which week of the year is this day in?
		int weekOfYear = (dayOfYear - dayOfYearOfWeek1Day1 + 7) / 7;

		harness.check(c.get(Calendar.WEEK_OF_YEAR) == weekOfYear);

		// Then we set YEAR + DAY_OF_WEEK + WEEK_OF_YEAR and
		// check we have MONTH and DAY_OF_MONTH correct.
		
		d.clear();
		d.set(Calendar.YEAR, checkYear);
		d.set(Calendar.DAY_OF_WEEK, dayOfWeek);
		d.set(Calendar.WEEK_OF_YEAR, weekOfYear);
		harness.check(d.get(Calendar.YEAR) == year);
		harness.check(d.get(Calendar.MONTH) == month);
		harness.check(d.get(Calendar.DAY_OF_MONTH) == day);
	      }
	    }
	  }
	}
      }
    }
  }
}
