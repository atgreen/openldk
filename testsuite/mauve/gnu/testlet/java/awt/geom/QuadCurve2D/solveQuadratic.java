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
 * Checks whether the QuadCurve2D.solveQuadratic methods work
 * correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class solveQuadratic
  implements Testlet
{
  private static final double EPSILON = 1e-6;


  private TestHarness harness;

  public void test(TestHarness harness)
  {
    this.harness = harness;

    // x - 23.2 = 0 --> x = 23.2
    checkSolutions(0, 1, -23.2, new double[]{23.2});

    // 8x + 8 = 0 --> x = -1
    checkSolutions(0, 8, 8, new double[]{-1});

    // 4x^2 = 0 --> x = 0
    // Classpath bug #6095
    checkSolutions(4, 0, 0, new double[]{0});

    // (x^2)/2 - 2 = 0 --> x = {-2, 2}
    checkSolutions(0.5, 0, -2, new double[]{-2, 2});

    // -x^2 + 4x = 0 --> x = {0, 4}
    checkSolutions(-1, 4, 0, new double[]{0, 4});

    // (x^2)/10 + 20x + 1000 = 0
    //
    // Sun J2SE 1.4.1_01 (GNU/Linux i386) reports the result -100
    // twice, which is not correct.
    //
    // The GNU Classpath implementation of 2004-01-07 seems to have a
    // numeric stability problem that manifests with some VMs (such as
    // gcj on IA-32), but not with others (such as jamvm, or Sun J2SE
    // 1.4.1_01 executing the Classpath implementation).
    // See Classpath bug #7123.
    checkSolutions(.1, 20, 1000, new double[]{-100});

    // Test case for b^2 >> 4ac
    checkSolutions(.1, 2000, .2,
                   new double[] { -19999.999899999995, 
                                  -1.0000000050000002E-4 });

    // 10x^2 + 3x + 5 = 0 --> no solution
    checkSolutions(10, 3, 5, new double[0]);

    // 2x^2 + 2x + 2 --> no solution
    checkSolutions(2, 1, 2, new double[0]);

    // 0 = 0
    checkSolutions(0, 0, 0, null);

    // 123 = 0
    checkSolutions(0, 0, 123, null);

    // The subsequent five tests are taken from test code in the
    // GNU Scientific Library (GSL), which is licensed under the
    // GNU General Public License version 2 or later.
    // See file "gsl/poly/test.c", revision 1.16 of 2003-07-26.
    // Original author: Brian Gough <bjg@network-theory.co.uk>
    checkSolutions(4, -20, 26, new double[0]);
    checkSolutions(4, -20, 25, new double[]{ 2.5 });
    checkSolutions(4, -20, 21, new double[]{ 1.5, 3.5 });
    checkSolutions(4, 7, 0, new double[]{ -1.75, 0 });
    checkSolutions(5, 0, -20, new double[]{ -2, 2 });
  }


  /**
   * Checks whether all expected solutions were found.
   *
   * @param c2 the coefficient for x^2.
   *
   * @param c1 the coefficient for x^1.
   *
   * @param c0 the coefficient for x^0.
   *
   * @param expected the expected set of solutions, or
   * <code>null</code> if QuadCurve2D.solveQuadratic is expected to
   * return -1.
   */
  private void checkSolutions(double c2, double c1, double c0,
                              double[] expected)
  {
    double[] solutions = new double[2];
    int numSols, numExpectedSolutions;
    StringBuffer buf = new StringBuffer();
    boolean ok = false;

    if (c2 != 0)
    {
      buf.append(c2);
      buf.append("x^2");
    }
    if (c1 != 0)
    {
      buf.append(c1 > 0 ? " + " : " - ");
      buf.append(Math.abs(c1));
      buf.append("x");
    }
    if (c0 != 0)
    {
      buf.append(c0 > 0 ? " + " : " - ");
      buf.append(Math.abs(c0));
    }
    buf.append(" = 0");
    harness.checkPoint(buf.toString());

    // Check #1: Number of actual solutions == number of expected solutions?
    numExpectedSolutions = expected == null ? -1 : expected.length;
    numSols = QuadCurve2D.solveQuadratic(new double[] { c0, c1, c2 },
                                         solutions);
    ok = numSols == numExpectedSolutions;
    harness.check(ok);

    // Check #2: All solutions found?
    for (int i = 0; i < numExpectedSolutions; i++)
    {
      boolean found = false;

      for (int j = 0; j < numSols; j++)
      {
        if (Math.abs(solutions[j] - expected[i]) < EPSILON)
        {
          found = true;
          break;
        }
      }

      if (!found)
      {
        harness.debug("solution " + expected[i] + " not found");
        ok = false;
      }
    }
    harness.check(ok);

    // Dump the arrays for debugging.
    if (!ok)
    {
      harness.debug("  got " + makeString(solutions));
      harness.debug("  expected " + makeString(expected));
    }
  }


  /**
   * Produces a String representation for a double[].
   */
  private static String makeString(double[] arr)
  {
    StringBuffer buf = new StringBuffer(50);

    if (arr == null)
      return "null";

    buf.append('[');
    for (int i = 0; i < arr.length; i++)
    {
      if (i > 0)
        buf.append(", ");
      buf.append(arr[i]);
    }
    buf.append(']');
    return buf.toString();
  }
}
