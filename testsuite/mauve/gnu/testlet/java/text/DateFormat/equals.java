// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.java.text.DateFormat;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.SimpleTimeZone;
import java.util.TimeZone;

/**
 * Some checks for the equals() method in the DateFormat class.  Bug 5066247 is
 * a general request for a better API specification.  The following bug reports
 * provide some clues about how the API is supposed to behave:
 * 
 * http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4071441
 * http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4072858
 */
public class equals implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not allowed).
   */
  public void test(TestHarness harness) 
  {
    DateFormat f1 = new SimpleDateFormat("yyyy");
    DateFormat f2 = new SimpleDateFormat("yyyy");
    harness.check(f1.equals(f2));                  // check 1
    harness.check(f2.equals(f1));                  // check 2
    
    // check calendar differences:
    GregorianCalendar c1 = new GregorianCalendar(TimeZone.getTimeZone("GMT"), 
        Locale.UK);
    GregorianCalendar c2 = new GregorianCalendar(TimeZone.getTimeZone("GMT"), 
        Locale.UK);
    long millis = System.currentTimeMillis();
    c1.setTimeInMillis(millis);
    c2.setTimeInMillis(millis);
    f1.setCalendar(c1);
    f2.setCalendar(c2);
    harness.check(f1.equals(f2));
    
    // timeInMillis --> guessing that differences should be ignored
    c1.setTimeInMillis(123L);
    harness.check(f1.equals(f2));    // still equal!
    c2.setTimeInMillis(123L);
    harness.check(f1.equals(f2));
    
    // firstDayOfWeek
    c1.setFirstDayOfWeek(Calendar.THURSDAY);
    harness.check(!f1.equals(f2));
    c2.setFirstDayOfWeek(Calendar.THURSDAY);
    harness.check(f1.equals(f2));
    
    // minimalDaysInFirstWeek
    c1.setMinimalDaysInFirstWeek(6);
    harness.check(!f1.equals(f2));
    c2.setMinimalDaysInFirstWeek(6);
    harness.check(f1.equals(f2));
    
    // timeZone
    c1.setTimeZone(new SimpleTimeZone(0, "Z1"));
    harness.check(!f1.equals(f2));
    c2.setTimeZone(new SimpleTimeZone(0, "Z1"));
    harness.check(f1.equals(f2));
    
    // locale
    c1 = new GregorianCalendar(TimeZone.getTimeZone("GMT"), Locale.UK);
    c2 = new GregorianCalendar(TimeZone.getTimeZone("GMT"), Locale.US);
    c1.setTimeInMillis(millis);
    c2.setTimeInMillis(millis);
    f1.setCalendar(c1);
    f2.setCalendar(c2);
    harness.check(!f1.equals(f2));
    c2 = new GregorianCalendar(TimeZone.getTimeZone("GMT"), Locale.UK);
    c2.setTimeInMillis(millis);
    f2.setCalendar(c2);
    harness.check(f1.equals(f2));

    // gregorianChange
    c1.setGregorianChange(new Date(123L));
    harness.check(f1.equals(f2));
    c2.setGregorianChange(new Date(123L));
    harness.check(f1.equals(f2));

  }

}
