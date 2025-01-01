//Tags: JDK1.1

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

package gnu.testlet.java.awt.Rectangle;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Rectangle;

/**
 * Checks that the intersects() method works correctly.
 */
public class intersects implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    Rectangle r0 = new Rectangle(0, 0, 0, 0);
    Rectangle r1 = new Rectangle(0, 0, 1, 1);
    Rectangle r2 = new Rectangle(1, 1, 1, 1);
    Rectangle r3 = new Rectangle(-1, -1, 1, 1);
    Rectangle r4 = new Rectangle(-1, -1, 2, 2);
    Rectangle r5 = new Rectangle(-1, -1, 3, 3);
    harness.check(!r0.intersects(r1));
    harness.check(!r0.intersects(r2));    
    harness.check(!r1.intersects(r2));
    harness.check(!r2.intersects(r3));
    harness.check(!r2.intersects(r4));
    harness.check(r2.intersects(r5));
    
    // check null argument
    boolean pass = false;
    try 
    {
      r0.intersects(null);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

}
