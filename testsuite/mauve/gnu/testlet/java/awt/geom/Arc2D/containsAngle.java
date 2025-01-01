//Tags: JDK1.2

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

package gnu.testlet.java.awt.geom.Arc2D;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.geom.Arc2D;
import java.awt.geom.Rectangle2D;

/**
 * Tests the containsAngle() method of the {@link Arc2D} class.
 */
public class containsAngle implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    Arc2D arc = new Arc2D.Double();
    harness.check(!arc.containsAngle(0.0));   // check 1

    arc = new Arc2D.Double(
      new Rectangle2D.Double(0.0, 0.0, 1.0, 1.0), 90.0, 90.0, Arc2D.PIE
    );
    harness.check(!arc.containsAngle(0.0));   // check 2
    harness.check(!arc.containsAngle(45.0));  // check 3 
    harness.check(arc.containsAngle(90.0));   // check 4
    harness.check(arc.containsAngle(135.0));  // check 5
    harness.check(!arc.containsAngle(180.0)); // check 6
    harness.check(!arc.containsAngle(225.0)); // check 7
  }

}
