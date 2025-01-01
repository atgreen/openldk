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

package gnu.testlet.java.awt.geom.CubicCurve2D;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.Point;
import java.awt.geom.Point2D;
import java.awt.geom.CubicCurve2D;


/**
 * Checks whether the various CubicCurve2D.setCurve methods work
 * correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class setCurve
  implements Testlet
{
  public void test(TestHarness harness)
  {
    test_array(harness);
    test_fourPoints(harness);
    test_PointArray(harness);
    test_CubicCurve2D(harness);
  }


  private void test_array(TestHarness harness)
  {
    CubicCurve2D curve;

    harness.checkPoint("array");

    curve = new CubicCurve2D.Double();
    curve.setCurve(new double[] { 1, 2, 3, 11, 12, 13, 14, 15, 16, 17, 18 },
                   /* offset */ 3);
    harness.check(curve.getX1(), 11);     // Check 1
    harness.check(curve.getY1(), 12);     // Check 2
    harness.check(curve.getCtrlX1(), 13); // Check 3
    harness.check(curve.getCtrlY1(), 14); // Check 4
    harness.check(curve.getCtrlX2(), 15); // Check 5
    harness.check(curve.getCtrlY2(), 16); // Check 6
    harness.check(curve.getX2(), 17);     // Check 7
    harness.check(curve.getY2(), 18);     // Check 8
  }


  private void test_fourPoints(TestHarness harness)
  {
    CubicCurve2D curve;

    harness.checkPoint("fourPoints");

    curve = new CubicCurve2D.Double();
    curve.setCurve(new Point2D.Float(1, 2),
                   new Point2D.Double(3, 4),
                   new Point(5, 6),
                   new Point2D.Float(7, 8));

    harness.check(curve.getX1(), 1);     // Check 1
    harness.check(curve.getY1(), 2);     // Check 2
    harness.check(curve.getCtrlX1(), 3); // Check 3
    harness.check(curve.getCtrlY1(), 4); // Check 4
    harness.check(curve.getCtrlX2(), 5); // Check 5
    harness.check(curve.getCtrlY2(), 6); // Check 6
    harness.check(curve.getX2(), 7);     // Check 7
    harness.check(curve.getY2(), 8);     // Check 8
  }


  private void test_PointArray(TestHarness harness)
  {
    CubicCurve2D curve;
    Point2D[] pts = new Point2D[20];

    harness.checkPoint("PointArray");

    curve = new CubicCurve2D.Double();
    pts[11] = new Point2D.Float(1,2);
    pts[12] = new Point2D.Double(3,4);
    pts[13] = new Point(5, 6);
    pts[14] = new Point(7, 8);
    curve.setCurve(pts, 11);

    harness.check(curve.getX1(), 1);     // Check 1
    harness.check(curve.getY1(), 2);     // Check 2
    harness.check(curve.getCtrlX1(), 3); // Check 3
    harness.check(curve.getCtrlY1(), 4); // Check 4
    harness.check(curve.getCtrlX2(), 5); // Check 5
    harness.check(curve.getCtrlY2(), 6); // Check 6
    harness.check(curve.getX2(), 7);     // Check 7
    harness.check(curve.getY2(), 8);     // Check 8
  }


  private void test_CubicCurve2D(TestHarness harness)
  {
    CubicCurve2D curve;
    Point2D[] pts = new Point2D[20];

    harness.checkPoint("CubicCurve2D");

    curve = new CubicCurve2D.Float();
    curve.setCurve(new CubicCurve2D.Double(9, 8, 7, 6, 5, 4, 3, 2));

    harness.check(curve.getX1(), 9);     // Check 1
    harness.check(curve.getY1(), 8);     // Check 2
    harness.check(curve.getCtrlX1(), 7); // Check 3
    harness.check(curve.getCtrlY1(), 6); // Check 4
    harness.check(curve.getCtrlX2(), 5); // Check 5
    harness.check(curve.getCtrlY2(), 4); // Check 6
    harness.check(curve.getX2(), 3);     // Check 7
    harness.check(curve.getY2(), 2);     // Check 8

    curve = new CubicCurve2D.Double();
    curve.setCurve(new CubicCurve2D.Float(90, 80, 70, 60, 50, 40, 30, 20));

    harness.check(curve.getX1(), 90);     // Check 9
    harness.check(curve.getY1(), 80);     // Check 10
    harness.check(curve.getCtrlX1(), 70); // Check 11
    harness.check(curve.getCtrlY1(), 60); // Check 12
    harness.check(curve.getCtrlX2(), 50); // Check 13
    harness.check(curve.getCtrlY2(), 40); // Check 14
    harness.check(curve.getX2(), 30);     // Check 15
    harness.check(curve.getY2(), 20);     // Check 16
  }
}
