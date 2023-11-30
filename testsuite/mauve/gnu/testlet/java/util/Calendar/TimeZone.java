/* TimeZone.java -- Regression test case for ensuring that setting timezone 
   recalculate field content
   Copyright (C) 2006 Olivier Jolly <olivier.jolly@pcedev.com>
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.1

package gnu.testlet.java.util.Calendar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests based on KiYun Roe bug report (PR 27343 in classpath bugzilla), which
 * ensures that when the timezone is changed, fields of the calendar are
 * recomputed
 * @author KiYun Roe <kiyun.roe@active-endpoints.com>
 * @author Olivier Jolly <olivier.jolly@pcedev.com>
 */
public class TimeZone implements Testlet
{

  public void test(TestHarness harness)
  {
    java.util.Calendar cal = java.util.Calendar.getInstance();
    cal.setTimeZone(java.util.TimeZone.getTimeZone("GMT+0"));
    cal.setTime(new java.util.Date());
    int hour1 = cal.get(java.util.Calendar.HOUR_OF_DAY);
    cal.setTimeZone(java.util.TimeZone.getTimeZone("GMT-5"));
    int hour2 = cal.get(java.util.Calendar.HOUR_OF_DAY);
    int delta = (hour1 - hour2 + 24) % 24;
    harness.check(delta, 5, "Check side effect of timezone setting");
  }

}
