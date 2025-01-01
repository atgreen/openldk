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

import java.awt.Point;
import java.awt.geom.Point2D;
import java.awt.geom.QuadCurve2D;


/**
 * Checks whether the various QuadCurve2D.setCurve methods work
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
    test_threePoints(harness);
    test_PointArray(harness);
    test_QuadCurve2D(harness);
  }


  private void test_array(TestHarness harness)
  {
    QuadCurve2D curve;

    harness.checkPoint("array");

    curve = new QuadCurve2D.Double();
    curve.setCurve(new double[] { 1, 2, 3, 11, 12, 13, 14, 15, 16, 17 },
                   /* offset */ 3);
    harness.check(curve.getX1(), 11);    // Check 1
    harness.check(curve.getY1(), 12);    // Check 2
    harness.check(curve.getCtrlX(), 13); // Check 3
    harness.check(curve.getCtrlY(), 14); // Check 4
    harness.check(curve.getX2(), 15);    // Check 5
    harness.check(curve.getY2(), 16);    // Check 6
  }


  private void test_threePoints(TestHarness harness)
  {
    QuadCurve2D curve;

    harness.checkPoint("threePoints");

    curve = new QuadCurve2D.Double();
    curve.setCurve(new Point2D.Float(1,2),
                   new Point2D.Double(3,4),
                   new Point(5, 6));

    harness.check(curve.getX1(), 1);    // Check 1
    harness.check(curve.getY1(), 2);    // Check 2
    harness.check(curve.getCtrlX(), 3); // Check 3
    harness.check(curve.getCtrlY(), 4); // Check 4
    harness.check(curve.getX2(), 5);    // Check 5
    harness.check(curve.getY2(), 6);    // Check 6
  }


  private void test_PointArray(TestHarness harness)
  {
    QuadCurve2D curve;
    Point2D[] pts = new Point2D[20];

    harness.checkPoint("PointArray");

    curve = new QuadCurve2D.Double();
    pts[11] = new Point2D.Float(1,2);
    pts[12] = new Point2D.Double(3,4);
    pts[13] = new Point(5, 6);
    curve.setCurve(pts, 11);

    harness.check(curve.getX1(), 1);    // Check 1
    harness.check(curve.getY1(), 2);    // Check 2
    harness.check(curve.getCtrlX(), 3); // Check 3
    harness.check(curve.getCtrlY(), 4); // Check 4
    harness.check(curve.getX2(), 5);    // Check 5
    harness.check(curve.getY2(), 6);    // Check 6
  }


  private void test_QuadCurve2D(TestHarness harness)
  {
    QuadCurve2D curve;
    Point2D[] pts = new Point2D[20];

    harness.checkPoint("QuadCurve2D");

    curve = new QuadCurve2D.Float();
    curve.setCurve(new QuadCurve2D.Double(9, 8, 7, 6, 5, 4));

    harness.check(curve.getX1(), 9);    // Check 1
    harness.check(curve.getY1(), 8);    // Check 2
    harness.check(curve.getCtrlX(), 7); // Check 3
    harness.check(curve.getCtrlY(), 6); // Check 4
    harness.check(curve.getX2(), 5);    // Check 5
    harness.check(curve.getY2(), 4);    // Check 6

    curve = new QuadCurve2D.Double();
    curve.setCurve(new QuadCurve2D.Float(90, 80, 70, 60, 50, 40));

    harness.check(curve.getX1(), 90);    // Check 7
    harness.check(curve.getY1(), 80);    // Check 8
    harness.check(curve.getCtrlX(), 70); // Check 9
    harness.check(curve.getCtrlY(), 60); // Check 10
    harness.check(curve.getX2(), 50);    // Check 11
    harness.check(curve.getY2(), 40);    // Check 12
  }
}
