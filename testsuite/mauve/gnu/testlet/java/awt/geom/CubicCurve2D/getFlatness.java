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
 * Checks whether the various CubicCurve2D.getFlatness methods work
 * correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class getFlatness
  implements Testlet
{
  private TestHarness harness;

  public void test(TestHarness harness)
  {
    this.harness = harness;

    // Check 1: eight doubles, collinear
    chkeps(CubicCurve2D.getFlatness(1, 2, 3, 4, 5, 6, 7, 8), 0);

    // Check 2: eight doubles, collinear (C1 and C2 swapped)
    chkeps(CubicCurve2D.getFlatness(1, 2, 5, 6, 3, 4, 7, 8), 0);

    // Check 3: eight doubles, collinear (C2 and P2 swapped)
    chkeps(CubicCurve2D.getFlatness(1, 2, 3, 4, 7, 8, 5, 6),
           2.8284271247461903);

    // Check 4: eight doubles, not collinear
    chkeps(CubicCurve2D.getFlatness(10, -20, 3, 4, 5, 6, 40, 0),
           24.40680863391008);

    // Check 5: double[], not collinear
    double[] d = new double[] {2, 100, -200, 30, 44, 5, 600, 77, 18981};
    chkeps(CubicCurve2D.getFlatness(d, 1), 94.04064976860437);

    // Check 6: Method on CubicCurve2D, degenerated to point
    chkeps((new CubicCurve2D.Double()).getFlatness(), 0);

    // Check 7: Method on CubicCurve2D, not collinear
    chkeps((new CubicCurve2D.Double(9,8,1,2,-4,0,1311,2332)).getFlatness(),
           15.264337522473747);
  }


  private void chkeps(double a, double b)
  {
    if (Math.abs(a - b) > 1e-7)
      harness.check(a, b);
    else
      harness.check(true);
  }
}
