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

package gnu.testlet.java.awt.geom.CubicCurve2D.Double;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.awt.geom.CubicCurve2D;


/**
 * Checks whether the CubicCurve2D.Double.getCtrlP1() method works
 * correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class getCtrlP1
  implements Testlet
{
  public void test(TestHarness harness)
  {
    CubicCurve2D.Double curve;

    curve = new CubicCurve2D.Double(-1, -2, -3.141, 42, -5, -6, -77, 88);
    harness.check(curve.getCtrlP1().getX(), -3.141); // Check 1
    harness.check(curve.getCtrlP1().getY(), 42)   ;  // Check 2
  }
}
