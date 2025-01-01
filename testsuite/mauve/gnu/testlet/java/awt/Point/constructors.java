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

package gnu.testlet.java.awt.Point;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Point;

/**
 * Some checks for the constructors in the {@link Point} class.
 */
public class constructors implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
  }

  private void testConstructor1(TestHarness harness)
  {
    Point p = new Point();
    harness.check(p.x, 0);
    harness.check(p.y, 0);
  }
  
  private void testConstructor2(TestHarness harness)
  {
    Point p = new Point(1, 2);
    harness.check(p.x, 1);
    harness.check(p.y, 2);
  }
  
  private void testConstructor3(TestHarness harness) 
  {
    Point p = new Point(new Point(2, 3));
    harness.check(p.x, 2);
    harness.check(p.y, 3);
    
    try
    {
      p = new Point(null);
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }
  }

}
