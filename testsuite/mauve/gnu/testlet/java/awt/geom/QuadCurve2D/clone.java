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

package gnu.testlet.java.awt.geom.QuadCurve2D;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.geom.QuadCurve2D;


/**
 * Checks whether the QuadCurve2D.clone method works correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class clone
  implements Testlet
{
  public void test(TestHarness harness)
  {
    QuadCurve2D curve;

    // Checks 1 to 7: Clone a QuadCurve2D.Double
    curve = (QuadCurve2D) (new QuadCurve2D.Double(4,3,9,1,7,8)).clone();
    harness.check(curve instanceof QuadCurve2D.Double); // 1
    harness.check(curve.getX1(), 4.0);                  // 2
    harness.check(curve.getY1(), 3.0);                  // 3
    harness.check(curve.getCtrlX(), 9.0);               // 4
    harness.check(curve.getCtrlY(), 1.0);               // 5
    harness.check(curve.getX2(), 7.0);                  // 6
    harness.check(curve.getY2(), 8.0);                  // 7

    // Checks 8 to 14: Clone a QuadCurve2D.Float
    curve = (QuadCurve2D) (new QuadCurve2D.Float(-4,3,-9,1,7,-8)).clone();
    harness.check(curve instanceof QuadCurve2D.Float);  // 1
    harness.check(curve.getX1(), -4.0);                 // 2
    harness.check(curve.getY1(), 3.0);                  // 3
    harness.check(curve.getCtrlX(), -9.0);              // 4
    harness.check(curve.getCtrlY(), 1.0);               // 5
    harness.check(curve.getX2(), 7.0);                  // 6
    harness.check(curve.getY2(), -8.0);                 // 7
  }
}
