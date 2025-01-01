// Tags: JDK1.1

// Copyright (C) 2005 David Daney <ddaney@avtrex.com>

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

/**
 * Some checks for the getMinimum() method in the 
 * {@link GregorianCalendar} class.
 */
public class getMinimum implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)   
  {
    testX(harness);
  }
  
  /**
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  private void testX(TestHarness harness) {
    Calendar c1 = new GregorianCalendar();
    harness.check(c1.getMinimum(Calendar.HOUR_OF_DAY), 0);    
    harness.check(c1.getMinimum(Calendar.MINUTE), 0);    
    harness.check(c1.getMinimum(Calendar.SECOND), 0);    
    harness.check(c1.getMinimum(Calendar.MILLISECOND), 0);    
  }
}
