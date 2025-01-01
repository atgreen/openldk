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
 * Checks whether the various QuadCurve2D.getFlatnessSq methods work
 * correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class getFlatnessSq
  implements Testlet
{
  private TestHarness harness;

  public void test(TestHarness harness)
  {
    this.harness = harness;

    // Checks 1 and 2: six doubles
    chkeps(QuadCurve2D.getFlatnessSq(1, 2, 3, 4, 5, 6), 0);        // 1
    chkeps(QuadCurve2D.getFlatnessSq(10, -20, 3, 4, 5, 6),         // 2
           5.483594864479315);

    // Check 3: double[] 
    double[] d = new double[] {2, 100, -200, 30, 44, 5, 600};
    chkeps(QuadCurve2D.getFlatnessSq(d, 1), 1659.6470089749782);   // 3

    // Checks 4 and 5: Method on QuadCurve2D
    chkeps((new QuadCurve2D.Double()).getFlatnessSq(), 0);         // 4
    chkeps((new QuadCurve2D.Double(9,8,1,2,-4,0)).getFlatnessSq(), // 5
           0.8412017167381975);
  }


  private void chkeps(double a, double b)
  {
    if (Math.abs(a - b) > 1e-5)
      harness.check(a, b);
    else
      harness.check(true);
  }
}
