// Test Calendar.add().

// Copyright (c) 2001, 2006  Red Hat
// Written by Tom Tromey <tromey@redhat.com>

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

package gnu.testlet.java.util.Calendar;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.*;
import java.util.*;

public class add implements Testlet
{
  public void test (TestHarness harness)
  {
    java.util.TimeZone tz = java.util.TimeZone.getTimeZone ("GMT");
    DateFormat cdf = new SimpleDateFormat ("EEE, d MMM yyyy HH:mm:ss zzz",
					   Locale.US);
    cdf.setTimeZone (tz);

    // We use the US locale since we need a fixed one, and this one I
    // understand.
    Calendar k = Calendar.getInstance (tz, Locale.US);
    Date epoch = new Date (0);
    k.setTime (epoch);

    // Just double-check to make sure the tests themselves are ok.
    harness.check (cdf.format (epoch), "Thu, 1 Jan 1970 00:00:00 GMT");
    harness.check (k.getTime (), epoch);

    // No-op.
    k.add (Calendar.YEAR, 0);
    harness.check (k.getTime (), epoch, "no-op add()");

    k.add (Calendar.YEAR, 12);
    harness.check (k.get (Calendar.YEAR), 1982, "add() to year");
    k.add (Calendar.YEAR, -1);
    harness.check (k.get (Calendar.YEAR), 1981);
    // Month or hour shouldn't change.
    harness.check (k.get (Calendar.MONTH), 0);
    harness.check (k.get (Calendar.HOUR_OF_DAY), 0);

    // Update the hour and the day should change.
    k.add (Calendar.HOUR_OF_DAY, 30);
    harness.check (k.get (Calendar.HOUR_OF_DAY), 6, "add() to hour");
    harness.check (k.get (Calendar.DATE), 2);
    k.add (Calendar.HOUR_OF_DAY, -20);
    harness.check (k.get (Calendar.HOUR_OF_DAY), 10);
    harness.check (k.get (Calendar.DATE), 1);

    // Update the month and the year should change.
    k.add (Calendar.MONTH, -13);
    harness.check (k.get (Calendar.MONTH), Calendar.DECEMBER,
		   "add() to month");
    harness.check (k.get (Calendar.YEAR), 1979);
    k.add (Calendar.MONTH, 2);
    harness.check (k.get (Calendar.MONTH), Calendar.FEBRUARY);
    harness.check (k.get (Calendar.YEAR), 1980);
    harness.check (k.get (Calendar.DATE), 1);

    // 1980 was a leap year.
    k.add (Calendar.DATE, 28);
    harness.check (k.get (Calendar.MONTH), Calendar.FEBRUARY, "leap year");
    harness.check (k.get (Calendar.DATE), 29);
    k.add (Calendar.DATE, 1);
    harness.check (k.get (Calendar.MONTH), Calendar.MARCH);
    harness.check (k.get (Calendar.DATE), 1);

    harness.check (cdf.format (k.getTime ()),
		   "Sat, 1 Mar 1980 10:00:00 GMT",
		   "wrap up");
    
    testPreviousDate(harness);
    
  }
  
  public void testPreviousDate(TestHarness harness)
  {
    Calendar calendar = Calendar.getInstance();
    Date now = new Date();

    // Calculate the start of today.
    calendar.setTime(now);
    calendar.clear(Calendar.HOUR_OF_DAY);
    calendar.clear(Calendar.MINUTE);
    calendar.clear(Calendar.SECOND);
    calendar.clear(Calendar.MILLISECOND);

    Date todayStart = calendar.getTime();

    // Calculate the start of yesterday.
    calendar.setTime(now);
    calendar.add(Calendar.DATE, -1); // this change shouldn't be lost
    calendar.clear(Calendar.HOUR_OF_DAY);
    calendar.clear(Calendar.MINUTE);
    calendar.clear(Calendar.SECOND);
    calendar.clear(Calendar.MILLISECOND);

    Date yesterdayStart = calendar.getTime();

    harness.check(yesterdayStart.before(todayStart),
                  "PR27362: Check that clear didn't swallow a previous add() call");
  }
  
}
