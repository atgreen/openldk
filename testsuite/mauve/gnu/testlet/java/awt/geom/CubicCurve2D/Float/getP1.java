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

package gnu.testlet.java.awt.geom.CubicCurve2D.Float;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.awt.geom.CubicCurve2D;


/**
 * Checks whether the CubicCurve2D.Float.getP1() method works
 * correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class getP1
  implements Testlet
{
  public void test(TestHarness harness)
  {
    CubicCurve2D.Float curve;

    curve = new CubicCurve2D.Float(-1.2e8f, -2.3e7f, -3, -4, -5, -6, -7, -8);
    harness.check(curve.getP1().getX(), -1.2e8f); // Check 1
    harness.check(curve.getP1().getY(), -2.3e7f); // Check 2
  }
}
