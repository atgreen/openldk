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
 * Checks whether the various QuadCurve2D.getFlatness methods work
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

    // Check 1: six doubles, collinear
    chkeps(QuadCurve2D.getFlatness(1, 2, 3, 4, 5, 6), 0);

    // Check 2: six doubles, collinear
    chkeps(QuadCurve2D.getFlatness(1, 2, 5, 6, 3, 4),
           2.8284271247461903);

    // Check 3: six doubles, not collinear
    chkeps(QuadCurve2D.getFlatness(10, -20, 3, 4, 5, 6),
           2.3417076812615436);

    // Check 4: double[], not collinear
    double[] d = new double[] {2, 100, -200, 30, 44, 5, 600};
    chkeps(QuadCurve2D.getFlatness(d, 1), 40.73876543263159);

    // Check 5: Method on QuadCurve2D, collinear
    chkeps((new QuadCurve2D.Double()).getFlatness(), 0);

    // Check 6: Method on QuadCurve2D, not collinear
    chkeps((new QuadCurve2D.Double(9,8,1,2,-4,0)).getFlatness(),
           0.9171704949125857);
  }


  private void chkeps(double a, double b)
  {
    if (Math.abs(a - b) > 1e-6)
      harness.check(a, b);
    else
      harness.check(true);
  }
}
