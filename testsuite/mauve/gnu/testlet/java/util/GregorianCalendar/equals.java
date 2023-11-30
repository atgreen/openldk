// Tags: JDK1.1

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.java.util.GregorianCalendar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.SimpleTimeZone;

/**
 * Some checks for the equals() method in the {@link GregorianCalendar} class.
 * Note that the spec has been filled out in the 1.5 API docs.
 */
public class equals implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)   
  {
    long millis = System.currentTimeMillis(); 
    GregorianCalendar c1 = new GregorianCalendar();
    GregorianCalendar c2 = new GregorianCalendar();
    c1.setTimeInMillis(millis);
    c2.setTimeInMillis(millis);
    harness.check(c1.equals(c2));
    harness.check(c2.equals(c1));
    
    c1.setTimeInMillis(0);
    harness.check(!c1.equals(c2));
    c2.setTimeInMillis(0);
    harness.check(c1.equals(c2));
    
    c1.setGregorianChange(new Date(Long.MIN_VALUE));
    harness.check(!c1.equals(c2));
    c2.setGregorianChange(new Date(Long.MIN_VALUE));
    harness.check(c1.equals(c2));

    c1.setFirstDayOfWeek(Calendar.WEDNESDAY);
    harness.check(!c1.equals(c2));
    c2.setFirstDayOfWeek(Calendar.WEDNESDAY);
    harness.check(c1.equals(c2));
    
    c1.setLenient(!c1.isLenient());
    harness.check(!c1.equals(c2));
    c2.setLenient(c1.isLenient());
    harness.check(c1.equals(c2));
    
    c1.setMinimalDaysInFirstWeek(6);
    harness.check(!c1.equals(c2));
    c2.setMinimalDaysInFirstWeek(6);
    harness.check(c1.equals(c2));

    c1.setTimeZone(new SimpleTimeZone(123, "123"));
    harness.check(!c1.equals(c2));
    c2.setTimeZone(new SimpleTimeZone(123, "123"));
    harness.check(c1.equals(c2));
  }
  
}
