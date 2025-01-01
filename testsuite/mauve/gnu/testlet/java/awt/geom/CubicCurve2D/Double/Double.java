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
 * Checks whether the CubicCurve2D.Double constructors works
 * correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class Double
  implements Testlet
{
  private TestHarness h;

  public void test(TestHarness harness)
  {
    this.h = harness;
    testZeroArgs();
    testEightArgs();
  }


  /**
   * Checks whether the zero-argument constructor works as specified.
   */
  public void testZeroArgs()
  {
    CubicCurve2D curve;

    h.checkPoint("noArgs");
    curve = new CubicCurve2D.Double();
    h.check(curve.getX1(), 0);      // 1
    h.check(curve.getY1(), 0);      // 2
    h.check(curve.getCtrlX1(), 0);  // 3
    h.check(curve.getCtrlY1(), 0);  // 4
    h.check(curve.getCtrlX2(), 0);  // 5
    h.check(curve.getCtrlY2(), 0);  // 6
    h.check(curve.getX2(), 0);      // 7
    h.check(curve.getY2(), 0);      // 8
  }


  /**
   * Checks whether the eight-argument constructor works as specified.
   */
  public void testEightArgs()
  {
    CubicCurve2D.Double curve;

    h.checkPoint("eightArgs");
    curve = new CubicCurve2D.Double(1e1, 2e2, 3e3, 4e4,
                                    5e5, 6e6, 7e7, 8e8);

    h.check(curve.getX1(), 1e1);      // 1
    h.check(curve.getY1(), 2e2);      // 2
    h.check(curve.getCtrlX1(), 3e3);  // 3
    h.check(curve.getCtrlY1(), 4e4);  // 4
    h.check(curve.getCtrlX2(), 5e5);  // 5
    h.check(curve.getCtrlY2(), 6e6);  // 6
    h.check(curve.getX2(), 7e7);      // 7
    h.check(curve.getY2(), 8e8);      // 8

    h.check(curve.x1, 1e1);           // 9
    h.check(curve.y1, 2e2);           // 10
    h.check(curve.ctrlx1, 3e3);       // 11
    h.check(curve.ctrly1, 4e4);       // 12
    h.check(curve.ctrlx2, 5e5);       // 13
    h.check(curve.ctrly2, 6e6);       // 14
    h.check(curve.x2, 7e7);           // 15
    h.check(curve.y2, 8e8);           // 16
  }
}
