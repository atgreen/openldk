// Tags: JDK1.2

// Copyright (C) 2003 Sascha Brawer <brawer@dandelis.ch>

// This file is part of Mauve.

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

package gnu.testlet.java.awt.geom.CubicCurve2D.Double;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.awt.geom.CubicCurve2D;


/**
 * Checks whether the CubicCurve2D.Double.setCurve method works
 * correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class setCurve
  implements Testlet
{
  public void test(TestHarness harness)
  {
    CubicCurve2D.Double curve;

    curve = new CubicCurve2D.Double(-1, -2, -3, -4, -5, -6, -7, -8);
    curve.setCurve(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8);
    harness.check(curve.x1, 1.1);     // Check 1
    harness.check(curve.y1, 2.2);     // Check 2
    harness.check(curve.ctrlx1, 3.3); // Check 3
    harness.check(curve.ctrly1, 4.4); // Check 4
    harness.check(curve.ctrlx2, 5.5); // Check 5
    harness.check(curve.ctrly2, 6.6); // Check 6
    harness.check(curve.x2, 7.7);     // Check 7
    harness.check(curve.y2, 8.8);     // Check 8
  }
}
