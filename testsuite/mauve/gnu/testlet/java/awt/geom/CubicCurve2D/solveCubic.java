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


/**
 * Checks whether the CubicCurve2D.solveCubic methods work
 * correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class solveCubic
  implements Testlet
{
  private TestHarness harness;

  public void test(TestHarness harness)
  {
    this.harness = harness;

    checkSolutions(1, 0, 0, -1, new double[]{1});
    checkSolutions(5, 6, 1, 0, new double[]{0, -1, -0.2});
    checkSolutions(8, 2, -1, 0, new double[]{0, -0.5, 0.25});
    checkSolutions(9, -6, 0.5, -1, new double[]{0.7785994720166718});
    
    // Sun J2SE 1.4.1_01 (GNU/Linux IA-32) fails to find x=0 here.
    checkSolutions(0.25, -1, 0, 0, new double[]{0, 4});
    checkSolutions(1, 1, 0, 0, new double[]{-1, 0});
    checkSolutions(1, 0.5, 0, 0, new double[]{-0.5, 0});

    // Some equations that are actually quadratic.
    checkSolutions(0, 0, 1, -23, new double[]{23});
    checkSolutions(0, 0, 0, 1968.5, null);
    checkSolutions(0, 0, 0, 0, null);
    checkSolutions(0, 0, 8, 8, new double[]{-1});
    checkSolutions(0, 0.5, 0, -2, new double[]{-2, 2});
    checkSolutions(0, 10, 3, 5, new double[0]);
    checkSolutions(0, 4, 0, 0, new double[]{0}); // Classpath bug #6095.

    // The subsequent six tests are taken from test code in the
    // GNU Scientific Library (GSL), which is licensed under the
    // GNU General Public License version 2 or later.
    // See file "gsl/poly/test.c", revision 1.16 of 2003-07-26.
    // Original author: Brian Gough <bjg@network-theory.co.uk>
    checkSolutions(1, 0, 0, -27, new double[]{3});
    checkSolutions(1, -51, 867, -4913, new double[]{17});
    checkSolutions(1, -57, 1071, -6647, new double[]{17, 23});
    checkSolutions(1, -11, -493, +6647, new double[]{17, -23});
    checkSolutions(1, -143, 5087, -50065, new double[]{17, 31, 95});
    checkSolutions(1, -109, 803, 50065, new double[]{-17, 31, 95});
  }

  
  /**
   * Checks whether all expected solutions were found.
   *
   * @param c3 the coefficient for x^3.
   *
   * @param c2 the coefficient for x^2.
   *
   * @param c1 the coefficient for x^1.
   *
   * @param c0 the coefficient for x^0.
   *
   * @param expected the expected set of solutions, or
   * <code>null</code> if CubicCurve2D.solveCubic is expected to
   * return -1.
   */
  private void checkSolutions(double c3, double c2, double c1, double c0,
                              double[] expected)
  {
    double[] solutions = new double[4];
    int numSols, numExpectedSolutions;
    StringBuffer buf = new StringBuffer();

    if (c3 != 0)
    {
      buf.append(c3);
      buf.append("x^3");
    }
    if (c2 != 0)
    {
      buf.append(c2 > 0 ? " + " : " - ");
      buf.append(Math.abs(c2));
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

    numExpectedSolutions = expected == null ? -1 : expected.length;
    numSols = CubicCurve2D.solveCubic(new double[] { c0, c1, c2, c3 },
                                      solutions);
    harness.check(numSols, numExpectedSolutions);

    for (int i = 0; i < numExpectedSolutions; i++)
    {
      boolean found = false;

      for (int j = 0; j < numSols; j++)
      {
        if (Math.abs(solutions[j] - expected[i]) < 1e-6)
        {
          found = true;
          break;
        }
      }

      harness.check(found);
      if (!found)
        harness.debug("solution " + expected[i] + " not found");
    }
  }
}
