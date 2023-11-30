//Tags: JDK1.0

//Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.Dimension;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Dimension;

/**
 * Checks that the equals() method in the {@link Dimension} class works 
 * correctly.
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
    Dimension d1 = new Dimension();
    Dimension d2 = new Dimension();
    harness.check(d1.equals(d2));
    harness.check(d2.equals(d1));
    harness.check(!d1.equals(null));
    
    d1.width = 5;
    harness.check(!d1.equals(d2));
    d2.width = 5;
    harness.check(d1.equals(d2));
    
    d1.height = 10;
    harness.check(!d1.equals(d2));
    d2.height = 10;
    harness.check(d1.equals(d2));
  }

}
