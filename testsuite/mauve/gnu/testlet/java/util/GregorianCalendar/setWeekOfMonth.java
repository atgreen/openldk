// Tags: JDK1.1

// Copyright (C) 2006 Mark J. Wielaard (mark@klomp.org)

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

public class setWeekOfMonth
  implements Testlet
{
  public void test(TestHarness harness)
  {
    GregorianCalendar cal;
    cal = new GregorianCalendar(TimeZone.getTimeZone("GMT"), Locale.US);

    // Month with 6 weeks
    cal.set(2006, Calendar.JULY, 30);
    cal.setLenient(false);

    harness.check(cal.getMaximum(Calendar.WEEK_OF_MONTH), 6);

    cal.clear(Calendar.DAY_OF_MONTH);
    cal.set(Calendar.WEEK_OF_MONTH, 1);
    cal.set(Calendar.DAY_OF_WEEK, Calendar.SATURDAY);
    harness.check(cal.get(Calendar.DAY_OF_MONTH), 1);

    cal.clear(Calendar.DAY_OF_MONTH);
    cal.set(Calendar.WEEK_OF_MONTH, 6);
    cal.set(Calendar.DAY_OF_WEEK, Calendar.MONDAY);
    harness.check(cal.get(Calendar.DAY_OF_MONTH), 31);

    // Month with 4 weeks
    cal.set(1998, Calendar.FEBRUARY, 14);
    cal.clear(Calendar.DAY_OF_MONTH);
    cal.set(Calendar.WEEK_OF_MONTH, 1);
    cal.set(Calendar.DAY_OF_WEEK, Calendar.SUNDAY);
    harness.check(cal.get(Calendar.DAY_OF_MONTH), 1);
    
    cal.clear(Calendar.DAY_OF_MONTH);
    cal.set(Calendar.WEEK_OF_MONTH, 4);
    cal.set(Calendar.DAY_OF_WEEK, Calendar.SATURDAY);
    harness.check(cal.get(Calendar.DAY_OF_MONTH), 28);
    harness.check(cal.get(Calendar.MONTH), Calendar.FEBRUARY);

    // Month with 5 weeks
    cal.set(1993, Calendar.FEBRUARY, 14);
    cal.clear(Calendar.DAY_OF_MONTH);
    cal.set(Calendar.WEEK_OF_MONTH, 5);
    cal.set(Calendar.DAY_OF_WEEK, Calendar.SUNDAY);
    harness.check(cal.get(Calendar.DAY_OF_MONTH), 28);
  }
}
