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

import java.awt.geom.CubicCurve2D;
import java.util.Arrays;

/**
 * Checks whether the CubicCurve2D.subdivide methods work
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
    src = new double[]{7,7,1.5,2,3,4,-5,6,9,13};
    left = new double[15];
    CubicCurve2D.subdivide(src, 2, left, 1, left, 7);
    harness.check(Arrays.equals(left,
                                new double[]{0, 1.5, 2, 2.25, 3, 0.625,
                                             4, 0.5625, 5.625, 0.5, 7.25,
                                             2, 9.5, 9, 13}));

    // Check 2: No exception for null results.
    try
    {
      CubicCurve2D.subdivide(src, 0, null, 0, null, 0);
      harness.check(true);
    }
    catch (Exception ex)
    {
      harness.check(false);
    }

    // Check 3 and 4: Degenerate case, subdividing a point (0,0)
    src = new double[8];
    left = new double[8];
    right = new double[8];
    CubicCurve2D.subdivide(src, 0, left, 0, right, 0);
    harness.check(Arrays.equals(left, new double[8]));
    harness.check(Arrays.equals(left, right));
  }


  private void test_curve2()
  {
    CubicCurve2D src, left, right;

    harness.checkPoint("curve2");
    src = new CubicCurve2D.Double(42, 24, 123, 321, -78011, -11087, 41, 28);
    left = new CubicCurve2D.Double();
    right = new CubicCurve2D.Float();
    src.subdivide(left, right);
    chkeps(left.getX1(), 42);            // 1
    chkeps(left.getY1(), 24);            // 2
    chkeps(left.getCtrlX1(), 82.5);      // 3
    chkeps(left.getCtrlY1(), 172.5);     // 4
    chkeps(left.getCtrlX2(), -19430.75); // 5
    chkeps(left.getCtrlY2(), -2605.25);  // 6
    chkeps(left.getX2(), -29197.625);    // 7
    chkeps(left.getY2(), -4030.75);      // 8
    chkeps(right.getX1(), -29197.625);   // 9
    chkeps(right.getY1(), -4030.75);     // 10
    chkeps(right.getCtrlX1(), -38964.5); // 11
    chkeps(right.getCtrlY1(), -5456.25); // 12
    chkeps(right.getCtrlX2(), -38985.0); // 13
    chkeps(right.getCtrlY2(), -5529.5);  // 14
    chkeps(right.getX2(), 41);           // 15
    chkeps(right.getY2(), 28);           // 16
  }


  private void test_curve3()
  {
    CubicCurve2D src, left, right;

    harness.checkPoint("curve3");
    src = new CubicCurve2D.Double(23, -23, 42, -42, 1968.5, -1968.5, 68, 96);
    left = new CubicCurve2D.Float();
    right = new CubicCurve2D.Float();
    CubicCurve2D.subdivide(src, left, right);
    chkeps(left.getX1(), 23);             // 1
    chkeps(left.getY1(), -23);            // 2
    chkeps(left.getCtrlX1(), 32.5);       // 3
    chkeps(left.getCtrlY1(), -32.5);      // 4
    chkeps(left.getCtrlX2(), 518.875);    // 5
    chkeps(left.getCtrlY2(), -518.875);   // 6
    chkeps(left.getX2(), 765.3125);       // 7
    chkeps(left.getY2(), -744.8125);      // 8
    chkeps(right.getX1(), 765.3125);      // 9
    chkeps(right.getY1(), -744.8125);     // 10
    chkeps(right.getCtrlX1(), 1011.75);   // 11
    chkeps(right.getCtrlY1(), -970.75);   // 12
    chkeps(right.getCtrlX2(), 1018.25);   // 13
    chkeps(right.getCtrlY2(), -936.25) ;  // 14
    chkeps(right.getX2(), 68);            // 15
    chkeps(right.getY2(), 96);            // 16

    CubicCurve2D.subdivide(left, null, left);
    chkeps(left.getX1(), 305.3046875);     // 17
    chkeps(left.getY1(), -302.7421875);    // 18
    chkeps(left.getCtrlX1(), 458.890625);  // 19
    chkeps(left.getCtrlY1(), -453.765625); // 20
    chkeps(left.getCtrlX2(), 642.09375);   // 21
    chkeps(left.getCtrlY2(), -631.84375);  // 22
    chkeps(left.getX2(), 765.3125);        // 23
    chkeps(left.getY2(), -744.8125);       // 24
  }


  private void chkeps(double a, double b)
  {
    if (Math.abs(a - b) > 1e-6)
      harness.check(a, b);
    else
      harness.check(true);
  }
}
