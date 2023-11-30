/* constructors.java -- some checks for the constructors in the 
                        SpinnerDateModel class.
   Copyright (C) 2006  David Gilbert <david.gilbert@object-refinery.com>
   
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

// Tags: JDK1.4

package gnu.testlet.javax.swing.SpinnerDateModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Calendar;
import java.util.Date;

import javax.swing.SpinnerDateModel;

/**
 * Some tests for the constructors in the {@link SpinnerDateModel} class.
 */
public class constructors implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testConstructor1(harness);
    testConstructor2(harness);
  }

  private void testConstructor1(TestHarness harness) 
  {
    harness.checkPoint("()");
    Date before = new Date();
    SpinnerDateModel m = new SpinnerDateModel();
    Date after = new Date();
    Date date = (Date) m.getValue();
    harness.check(date.getTime() >= before.getTime());
    harness.check(date.getTime() <= after.getTime());
    harness.check(m.getStart(), null);
    harness.check(m.getEnd(), null);
    harness.check(m.getCalendarField(), Calendar.DAY_OF_MONTH);
  }

  private void testConstructor2(TestHarness harness)  
  {
    harness.checkPoint("(Date, Comparable, Comparable, int)");
    Date now = new Date();
    Date start = new Date(now.getTime() - 1L);
    Date end = new Date(now.getTime() + 1L);
    Date preStart = new Date(now.getTime() - 2L);
    Date postEnd = new Date(now.getTime() + 2L);
    SpinnerDateModel m = new SpinnerDateModel(now, start, end, Calendar.YEAR);
    harness.check(m.getValue(), now);
    harness.check(m.getStart(), start);
    harness.check(m.getEnd(), end);
    harness.check(m.getCalendarField(), Calendar.YEAR);
    
    // value equal to start 
    m = new SpinnerDateModel(start, start, end, Calendar.YEAR);
    harness.check(m.getValue(), start);
    harness.check(m.getStart(), start);
    harness.check(m.getEnd(), end);
    harness.check(m.getCalendarField(), Calendar.YEAR);

    // value before start
    boolean pass = false;
    try
    {
      m = new SpinnerDateModel(preStart, start, end, Calendar.YEAR);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // value equal to end
    m = new SpinnerDateModel(end, start, end, Calendar.YEAR);
    harness.check(m.getValue(), end);
    harness.check(m.getStart(), start);
    harness.check(m.getEnd(), end);
    harness.check(m.getCalendarField(), Calendar.YEAR);
    
    // value above maximum
    pass = false;
    try
    {
      m = new SpinnerDateModel(postEnd, start, end, Calendar.YEAR);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // check null value
    pass = false;
    try
    {
      m = new SpinnerDateModel(null, start, end, Calendar.YEAR);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // check valid calendar fields
    m = new SpinnerDateModel(now, start, end, Calendar.ERA);
    harness.check(m.getCalendarField(), Calendar.ERA);
    m = new SpinnerDateModel(now, start, end, Calendar.YEAR);
    harness.check(m.getCalendarField(), Calendar.YEAR);
    m = new SpinnerDateModel(now, start, end, Calendar.MONTH);
    harness.check(m.getCalendarField(), Calendar.MONTH);
    m = new SpinnerDateModel(now, start, end, Calendar.WEEK_OF_YEAR);
    harness.check(m.getCalendarField(), Calendar.WEEK_OF_YEAR);
    m = new SpinnerDateModel(now, start, end, Calendar.WEEK_OF_MONTH);
    harness.check(m.getCalendarField(), Calendar.WEEK_OF_MONTH);
    m = new SpinnerDateModel(now, start, end, Calendar.DAY_OF_MONTH);
    harness.check(m.getCalendarField(), Calendar.DAY_OF_MONTH);
    m = new SpinnerDateModel(now, start, end, Calendar.DAY_OF_YEAR);
    harness.check(m.getCalendarField(), Calendar.DAY_OF_YEAR);
    m = new SpinnerDateModel(now, start, end, Calendar.DAY_OF_WEEK);
    harness.check(m.getCalendarField(), Calendar.DAY_OF_WEEK);
    m = new SpinnerDateModel(now, start, end, Calendar.DAY_OF_WEEK_IN_MONTH);
    harness.check(m.getCalendarField(), Calendar.DAY_OF_WEEK_IN_MONTH);
    m = new SpinnerDateModel(now, start, end, Calendar.AM_PM);
    harness.check(m.getCalendarField(), Calendar.AM_PM);
    m = new SpinnerDateModel(now, start, end, Calendar.HOUR);
    harness.check(m.getCalendarField(), Calendar.HOUR);
    m = new SpinnerDateModel(now, start, end, Calendar.HOUR_OF_DAY);
    harness.check(m.getCalendarField(), Calendar.HOUR_OF_DAY);
    m = new SpinnerDateModel(now, start, end, Calendar.MINUTE);
    harness.check(m.getCalendarField(), Calendar.MINUTE);
    m = new SpinnerDateModel(now, start, end, Calendar.SECOND);
    harness.check(m.getCalendarField(), Calendar.SECOND);
    m = new SpinnerDateModel(now, start, end, Calendar.MILLISECOND);
    harness.check(m.getCalendarField(), Calendar.MILLISECOND);
    m = new SpinnerDateModel(now, start, end, Calendar.DATE);
    harness.check(m.getCalendarField(), Calendar.DATE);
    
    // check invalid calendar fields

    pass = false;
    try
    {
      m = new SpinnerDateModel(now, start, end, Calendar.DST_OFFSET);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);

    pass = false;
    try
    {
      m = new SpinnerDateModel(now, start, end, Calendar.ZONE_OFFSET);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
  
  }

}
