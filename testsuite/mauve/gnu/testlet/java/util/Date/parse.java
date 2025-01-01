// Tags: JDK1.0

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Per Bothner <per@bothner.com>.

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

package gnu.testlet.java.util.Date;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.Date;

public class parse implements Testlet
{
  private TestHarness harness;

  static final long TZ_DIFF_MAX = 24*60*60*1000;

  public void test (TestHarness harness)
  {
    this.harness = harness;

    testParse("6 Sep 2003", 1062806400000L, TZ_DIFF_MAX);
    testParse("2003/9/6 9:30 PST", 1062869400000L, 2003, 9, 6);
    testParse("6 Sep 2003 9:30 PST", 1062869400000L, 2003, 9, 6);
    testParse("6 Sep 2003 9:30 AM PST", 1062869400000L, 2003, 9, 6);
    testParse("6 Sep 2003 9:30 pm EDT", 1062898200000L, 2003, 9, 6);
    testParse("6 Sep 2003 UTC", 1062806400000L, 2003, 9, 6);
    testParse("2/28/08 23:30 gmt", 1204241400000L, 2008, 2, 28);
  }

  /** Test Date.parse.
   * @param s a String argument to for Date.parse.
   * @param exp the expected time in milliseconds.
   * @param year the expected result of <code>new Date(exp).getYear()</code>.
   * @param month the expected result of <code>new Date(exp).getMonth()</code>.
   * @param date the expected result of <code>new Date(exp).getDate()</code>.
   * We allow up to one day "off" because the local timezone may differ
   * from UTC and/or the one specified.  (Hence don't call this
   * on the first or last day of the month.)
   */
  private void testParse (String s, long exp, int year, int month, int date)
  {
    long t = Date.parse(s);
    Date d = new Date(t);
    harness.checkPoint(s);
    harness.check(t, exp);
    harness.checkPoint(s+" .getYear");
    harness.check(1900 + d.getYear(), year);
    harness.checkPoint(s+" .getMonth");
    harness.check(1 + d.getMonth(), month);
    harness.checkPoint(s+" .getMonth");
    int dd = d.getDate();
    harness.checkPoint(s+" .getDate");
    harness.check(dd >= date - 1 && dd <= date + 1);
  }

  /** Test Date.parse.
   * @param s a String argument to for Date.parse.
   * @param exp the expected time in milliseconds.
   * @param fuzz allowable "error" in result due to 
   */
  private void testParse (String s, long exp, long fuzz)
  {
    harness.checkPoint(s);
    long t = Date.parse(s);
    harness.check(t >= exp - fuzz && t <= exp + fuzz);
  }
}

