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

package gnu.testlet.java.awt.Rectangle;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Rectangle;

/**
 * Checks that the union() method works correctly.
 */
public class union implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    Rectangle r1 = new Rectangle(1, 2, 3, 4);
    Rectangle r2 = new Rectangle(5, 6, 7, 8);
    Rectangle r3 = r1.union(r2);
    harness.check(r3.x == 1);
    harness.check(r3.y == 2);
    harness.check(r3.width == 11);
    harness.check(r3.height == 12);
    
    // check union with empty rectangle
    r1 = new Rectangle();
    r2 = new Rectangle(1, 2, 3, 4);
    r3 = r1.union(r2);
    harness.check(r3.x == 0);
    harness.check(r3.y == 0);
    harness.check(r3.width == 4);
    harness.check(r3.height == 6);    
    
    // check null argument
    boolean pass = false;
    try 
    {
      r3 = r1.union(null);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

}
