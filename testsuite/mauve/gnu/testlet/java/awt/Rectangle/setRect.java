// Tags: JDK1.2

// Copyright (C) 2004, 2006 David Gilbert <david.gilbert@object-refinery.com>

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.java.awt.Rectangle;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Rectangle;

/**
 * Checks that the setRect() method works correctly.
 */
public class setRect implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    Rectangle r1 = new Rectangle(1, 2, 3, 4);
    r1.setRect(5, 6, 7, 8);
    harness.check(r1.x == 5);
    harness.check(r1.y == 6);
    harness.check(r1.width == 7);
    harness.check(r1.height == 8);
  
    // test rounding - it seems that the code sets a rectangle that completely
    // encloses the double precision rectangle
    r1.setRect(5.9, 6.9, 7, 8);
    harness.check(r1.x, 5);
    harness.check(r1.y, 6);
    harness.check(r1.width, 8);
    harness.check(r1.height, 9);
    
    r1.setRect(-0.1, -0.2, 0.3, 0.4);
    harness.check(r1.x, -1);
    harness.check(r1.y, -1);
    harness.check(r1.width, 2);
    harness.check(r1.height, 2);
    
    // is negative width permitted?
    r1.setRect(1.0, 2.0, -3.0, 4.0);
    harness.check(r1.width, -3);
    harness.check(r1.isEmpty());
    
    // is negative height permitted?
    r1.setRect(1.0, 2.0, 3.0, -4.0);
    harness.check(r1.height, -4);
    harness.check(r1.isEmpty());
  }

}
