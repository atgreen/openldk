// Tags: JDK1.1

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

package gnu.testlet.java.util.GregorianCalendar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Some checks for the getMinimalDaysInFirstWeek() method in the 
 * {@link GregorianCalendar} class.
 */
public class getMinimalDaysInFirstWeek implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)   
  {
    test10546(harness);
  }
  
  /**
   * A test to support Classpath bug report 10456.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  private void test10546(TestHarness harness) {
      
    Calendar c1 = new GregorianCalendar(TimeZone.getTimeZone("Europe/London"), Locale.UK);
    c1.clear();
    c1.set(Calendar.YEAR, 2005);
    c1.set(Calendar.WEEK_OF_YEAR, 1);
    c1.set(Calendar.DAY_OF_WEEK, c1.getFirstDayOfWeek());
    harness.check(c1.getMinimalDaysInFirstWeek(), 4);    
    
    Calendar c2 = new GregorianCalendar(TimeZone.getTimeZone("Europe/Paris"), Locale.FRANCE);
    c2.clear();
    c2.set(Calendar.YEAR, 2005);
    c2.set(Calendar.WEEK_OF_YEAR, 1);
    c2.set(Calendar.DAY_OF_WEEK, c2.getFirstDayOfWeek());
    harness.check(c2.getMinimalDaysInFirstWeek(), 4);    
    
  }

}
