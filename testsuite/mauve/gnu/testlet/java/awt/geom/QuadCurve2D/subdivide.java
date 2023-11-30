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
import java.util.Arrays;

/**
 * Checks whether the QuadCurve2D.subdivide methods work
 * correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class subdivide
  implements Testlet
{
  private TestHarness harness;

  public void test(TestHarness harness)
  {
    this.harness = harness;
    test_array();
    test_curve2();
    test_curve3();
  }


  private void test_array()
  {
    double[] src, left, right;

    harness.checkPoint("array");

    // Check 1
    src = new double[]{7,7,1.5,2,3,4,-5,6};
    left = new double[11];
    QuadCurve2D.subdivide(src, 2, left, 1, left, 5);
    harness.check(Arrays.equals(left,
                                new double[]{0.0, 1.5, 2.0, 2.25, 3.0,
                                             0.625, 4.0, -1.0, 5.0,
                                             -5.0, 6.0}));

    // Check 2: No exception for null results.
    try
    {
      QuadCurve2D.subdivide(src, 0, null, 0, null, 0);
      harness.check(true);
    }
    catch (Exception ex)
    {
      harness.check(false);
    }

    // Check 3 and 4: Degenerate case, subdividing a point (0,0)
    src = new double[6];
    left = new double[6];
    right = new double[6];
    QuadCurve2D.subdivide(src, 0, left, 0, right, 0);
    harness.check(Arrays.equals(left, new double[6]));
    harness.check(Arrays.equals(left, right));
  }


  private void test_curve2()
  {
    QuadCurve2D src, left, right;

    harness.checkPoint("curve2");
    src = new QuadCurve2D.Double(42, 24, 123, 321, -78011, -11087);
    left = new QuadCurve2D.Double();
    right = new QuadCurve2D.Float();
    src.subdivide(left, right);
    chkeps(left.getX1(), 42);           // 1
    chkeps(left.getY1(), 24);           // 2
    chkeps(left.getCtrlX(), 82.5);      // 3
    chkeps(left.getCtrlY(), 172.5);     // 4
    chkeps(left.getX2(),-19430.75);     // 5
    chkeps(left.getY2(), -2605.25);     // 6
    chkeps(right.getX1(), -19430.75);   // 7
    chkeps(right.getY1(), -2605.25);    // 8
    chkeps(right.getCtrlX(), -38944.0); // 9
    chkeps(right.getCtrlY(), -5383.0);  // 10
    chkeps(right.getX2(), -78011);      // 11
    chkeps(right.getY2(), -11087);      // 12
  }


  private void test_curve3()
  {
    QuadCurve2D src, left, right;

    harness.checkPoint("curve3");
    src = new QuadCurve2D.Double(23, -23, 42, -42, 1968.5, -1968.5);
    left = new QuadCurve2D.Float();
    right = new QuadCurve2D.Float();
    QuadCurve2D.subdivide(src, left, right);
    chkeps(left.getX1(), 23);           // 1
    chkeps(left.getY1(), -23);          // 2
    chkeps(left.getCtrlX(), 32.5);      // 3
    chkeps(left.getCtrlY(), -32.5);     // 4
    chkeps(left.getX2(), 518.875);      // 5
    chkeps(left.getY2(), -518.875);     // 6
    chkeps(right.getX1(), 518.875);     // 7
    chkeps(right.getY1(), -518.875);    // 8
    chkeps(right.getCtrlX(), 1005.25);  // 9
    chkeps(right.getCtrlY(), -1005.25); // 10
    chkeps(right.getX2(), 1968.5);      // 11
    chkeps(right.getY2(), -1968.5);     // 12

    QuadCurve2D.subdivide(left, null, left);
    chkeps(left.getX1(), 151.71875);    // 13
    chkeps(left.getY1(), -151.71875);   // 14
    chkeps(left.getCtrlX(), 275.6875);  // 15
    chkeps(left.getCtrlY(), -275.6875); // 16
    chkeps(left.getX2(), 518.875);      // 17
    chkeps(left.getY2(), -518.875);     // 18
  }


  private void chkeps(double a, double b)
  {
    if (Math.abs(a - b) > 1e-6)
      harness.check(a, b);
    else
      harness.check(true);
  }
}
