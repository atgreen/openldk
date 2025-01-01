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

package gnu.testlet.java.awt.geom.QuadCurve2D.Float;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.awt.geom.QuadCurve2D;
import java.awt.geom.Rectangle2D;


/**
 * Checks whether the QuadCurve2D.Float.getBounds2D method works
 * correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class getBounds2D
  implements Testlet
{
  public void test(TestHarness harness)
  {
    QuadCurve2D.Float curve;
    Rectangle2D bounds;

    curve = new QuadCurve2D.Float(1, -2, -3, 4, 5, 7);
    bounds = curve.getBounds2D();
    harness.check(bounds instanceof Rectangle2D.Float);  // 1
    harness.check(bounds.getX(), -3);                    // 2
    harness.check(bounds.getY(), -2);                    // 3
    harness.check(bounds.getWidth(), 8);                 // 4
    harness.check(bounds.getHeight(), 9);                // 5
  }
}
