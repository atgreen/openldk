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
import java.util.GregorianCalendar;
import java.util.SimpleTimeZone;

/**
 * Some checks for the getOffset() methods in the SimpleTimeZone class.
 */
public class getOffset implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    testMethod1(harness);
    testMethod2(harness);
  }
  
  private void testMethod1(TestHarness harness) 
  {
    harness.checkPoint("(int, int, int, int, int, int)");
    int offset = -6 * 60 * 60 * 1000;
    int t1 = 5 * 60 * 60 * 1000;
    int t2 = 6 * 60 * 60 * 1000;
    int dst = 2 * 60 * 60 * 1000;
    SimpleTimeZone z = new SimpleTimeZone(offset, "Z1", 
      Calendar.APRIL, 26, 0, t1,
      Calendar.OCTOBER, 25, 0, t2, dst);
    harness.check(z.getOffset(GregorianCalendar.AD, 2004, Calendar.APRIL, 26, Calendar.MONDAY, t1 - 1000), offset);
    harness.check(z.getOffset(GregorianCalendar.AD, 2004, Calendar.APRIL, 26, Calendar.MONDAY, t1 + 1000), offset + dst);
    harness.check(z.getOffset(GregorianCalendar.AD, 2004, Calendar.OCTOBER, 25, Calendar.MONDAY, t2 - dst - 1000), offset + dst);
    harness.check(z.getOffset(GregorianCalendar.AD, 2004, Calendar.OCTOBER, 25, Calendar.MONDAY, t2 - dst + 1000), offset);    
  }

  private void testMethod2(TestHarness harness) 
  {
    harness.checkPoint("(Date)");
    int offset = -6 * 60 * 60 * 1000;
    int t1 = 5 * 60 * 60 * 1000;
    int t2 = 6 * 60 * 60 * 1000;
    int dst = 2 * 60 * 60 * 1000;
    SimpleTimeZone z = new SimpleTimeZone(offset, "Z1", 
      Calendar.APRIL, 26, 0, t1,
      Calendar.OCTOBER, 25, 0, t2, dst);
    GregorianCalendar c = new GregorianCalendar(z);
    c.set(2004, Calendar.APRIL, 26, 4, 59, 59);
    long d1 = c.getTimeInMillis();
    harness.check(z.getOffset(d1), offset);
    harness.check(z.getOffset(d1 + 2000), offset + dst);
    
    // to do : check end date  
  }
}
