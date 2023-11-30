// Tags: JDK1.4

// Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.java.util.SimpleTimeZone;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.SimpleTimeZone;
import java.util.TimeZone;

/**
 * Some checks for the inDaylightTime() method in the SimpleTimeZone class.
 */
public class inDaylightTime implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    test1(harness);
    test2(harness);
  }

  private void test1(TestHarness harness) 
  {
    int offset = -6 * 60 * 60 * 1000;
    int t1 = 5 * 60 * 60 * 1000;
    int t2 = 6 * 60 * 60 * 1000;
    int dst = 2 * 60 * 60 * 1000;
    SimpleTimeZone z = new SimpleTimeZone(offset, "Z1", 
      Calendar.APRIL, 26, 0, t1,
      Calendar.OCTOBER, 25, 0, t2, dst);
    
    GregorianCalendar c = new GregorianCalendar(z);
    c.set(2004, Calendar.APRIL, 25, 8, 0, 0);
    harness.check(!z.inDaylightTime(c.getTime()));
    c.set(2004, Calendar.APRIL, 26, 8, 0, 0);
    harness.check(z.inDaylightTime(c.getTime()));
    
    c.set(2004, Calendar.OCTOBER, 24, 8, 0, 0);
    harness.check(z.inDaylightTime(c.getTime()));
    c.set(2004, Calendar.OCTOBER, 25, 8, 0, 0);
    harness.check(!z.inDaylightTime(c.getTime()));
    
    try
    {
      boolean b = z.inDaylightTime(null);
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }
  }

  // Tests converted from the SimpleTimeZoneTest.java attachment
  // of http://gcc.gnu.org/ml/java-patches/2007-q1/msg00587.html.
  private void test2(TestHarness harness) 
  {
    TimeZone utc = (TimeZone) new SimpleTimeZone(0, "GMT");
    TimeZone.setDefault(utc);
    Calendar cal = Calendar.getInstance(utc);

    harness.checkPoint("test 1");

    TimeZone tz1 = new SimpleTimeZone(
      -12600000, "Canada/Newfoundland",
      Calendar.MARCH, 8, -Calendar.SUNDAY, 60000,
      Calendar.NOVEMBER, 1, -Calendar.SUNDAY, 60000);

    cal.set(2037, Calendar.NOVEMBER, 1, 2, 30, 0);
    harness.check(tz1.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2037, Calendar.NOVEMBER, 1, 2, 31, 0);
    harness.check(!tz1.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2038, Calendar.JANUARY, 1, 2, 29, 0);
    harness.check(!tz1.inDaylightTime(new Date(cal.getTimeInMillis())));

    harness.checkPoint("test 2");

    TimeZone tz2 = new SimpleTimeZone(
      -12600000, "Test1",
      Calendar.MARCH, 8, -Calendar.SUNDAY, 60000,
      SimpleTimeZone.WALL_TIME,
      Calendar.NOVEMBER, 1, -Calendar.SUNDAY, 60000,
      SimpleTimeZone.STANDARD_TIME,
      3600000);

    // NB this particular check fails on several proprietary JVMs
    // because the end time is interpreted with startTimeMode.
    cal.set(2037, Calendar.NOVEMBER, 1, 3, 30, 0);
    harness.check(tz2.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2037, Calendar.NOVEMBER, 1, 3, 31, 0);
    harness.check(!tz2.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2038, Calendar.JANUARY, 1, 3, 29, 0);
    harness.check(!tz2.inDaylightTime(new Date(cal.getTimeInMillis())));

    harness.checkPoint("test 3");

    TimeZone tz3 = new SimpleTimeZone(
      -12600000, "Test2",
      Calendar.MARCH, 8, -Calendar.SUNDAY, 60000,
      Calendar.NOVEMBER, 1, -Calendar.SUNDAY, 3660000,
      3600000);

    cal.set(2037, Calendar.NOVEMBER, 1, 3, 30, 0);
    harness.check(tz3.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2037, Calendar.NOVEMBER, 1, 3, 31, 0);
    harness.check(!tz3.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2038, Calendar.JANUARY, 1, 3, 29, 0);
    harness.check(!tz3.inDaylightTime(new Date(cal.getTimeInMillis())));

    harness.checkPoint("test 4");

    ((SimpleTimeZone) tz2).setEndRule(
      Calendar.NOVEMBER, 1, -Calendar.SUNDAY, 3660000);

    cal.set(2037, Calendar.NOVEMBER, 1, 3, 30, 0);
    harness.check(tz2.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2037, Calendar.NOVEMBER, 1, 3, 31, 0);
    harness.check(!tz2.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2038, Calendar.JANUARY, 1, 3, 29, 0);
    harness.check(!tz2.inDaylightTime(new Date(cal.getTimeInMillis())));

    harness.checkPoint("test 5");

    TimeZone tz4 = new SimpleTimeZone(
      -12600000, "Test1",
      Calendar.MARCH, 8, -Calendar.SUNDAY, 60000,
      SimpleTimeZone.STANDARD_TIME,
      Calendar.NOVEMBER, 1, -Calendar.SUNDAY, 60000,
      SimpleTimeZone.STANDARD_TIME,
      3600000);

    cal.set(2037, Calendar.NOVEMBER, 1, 3, 30, 0);
    harness.check(tz4.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2037, Calendar.NOVEMBER, 1, 3, 31, 0);
    harness.check(!tz4.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2038, Calendar.JANUARY, 1, 3, 29, 0);
    harness.check(!tz4.inDaylightTime(new Date(cal.getTimeInMillis())));

    harness.checkPoint("test 6");

    TimeZone tz5 = new SimpleTimeZone(
      -12600000, "Test3",
      Calendar.MARCH, 8, -Calendar.SUNDAY, 60000,
      Calendar.JANUARY, 1, 0, 60000,
      3600000);

    cal.set(2007, Calendar.DECEMBER, 31, 23, 59, 0);
    harness.check(tz5.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2007, Calendar.JANUARY, 1, 2, 29, 0);
    harness.check(tz5.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2007, Calendar.JANUARY, 1, 2, 30, 0);
    harness.check(tz5.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2007, Calendar.JANUARY, 1, 2, 31, 0);
    harness.check(!tz5.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2007, Calendar.JANUARY, 3, 2, 31, 0);
    harness.check(!tz5.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2007, Calendar.JANUARY, 3, 2, 31, 0);
    harness.check(!tz5.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2007, Calendar.MARCH, 11, 3, 30, 0);
    harness.check(!tz5.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2007, Calendar.MARCH, 11, 3, 31, 0);
    harness.check(tz5.inDaylightTime(new Date(cal.getTimeInMillis())));

    harness.checkPoint("test 7");

    TimeZone tz6 = new SimpleTimeZone(
      12600000, "Test4",
      Calendar.MARCH, 6, 0, 18000000 - 12600000,
      SimpleTimeZone.UTC_TIME,
      Calendar.NOVEMBER, -16, -Calendar.THURSDAY, 18000000 - 3600000-12600000,
      SimpleTimeZone.UTC_TIME,
      3600000);

    cal.set(2007, Calendar.MARCH, 6, 1, 29, 0);
    harness.check(!tz6.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2007, Calendar.MARCH, 6, 1, 30, 0);
    harness.check(tz6.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2007, Calendar.NOVEMBER, 15, 0, 29, 0);
    harness.check(tz6.inDaylightTime(new Date(cal.getTimeInMillis())));

    cal.set(2007, Calendar.NOVEMBER, 15, 0, 30, 0);
    harness.check(!tz6.inDaylightTime(new Date(cal.getTimeInMillis())));
  }

}
