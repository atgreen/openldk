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

package gnu.testlet.java.awt.geom.GeneralPath;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;


/**
 * Checks whether GeneralPath.getCurrentPoint works correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class getCurrentPoint
  implements Testlet
{
  public void test(TestHarness harness)
  {
    GeneralPath path = new GeneralPath();
    Point2D pt = new Point2D.Float();

    // Check 1
    harness.check(path.getCurrentPoint() == null);

    // Check 2
    path.moveTo(98, 76);
    pt.setLocation(98, 76);
    harness.check(pt.equals(path.getCurrentPoint()));

    // Check 3
    path.moveTo(12, 13.4f);
    pt.setLocation(12, 13.4f);
    harness.check(pt.equals(path.getCurrentPoint()));

    // Check 4
    path.lineTo(-1, -2);
    pt.setLocation(-1, -2);
    harness.check(pt.equals(path.getCurrentPoint()));

    // Check 5
    path.lineTo(-10, -20);
    pt.setLocation(-10, -20);
    harness.check(pt.equals(path.getCurrentPoint()));

    // Check 6
    path.quadTo(1, 2, 3, 4);
    pt.setLocation(3, 4);
    harness.check(pt.equals(path.getCurrentPoint()));

    // Check 7
    path.curveTo(5, 6, 7, 8, 9, 10);
    pt.setLocation(9, 10);
    harness.check(pt.equals(path.getCurrentPoint()));

    // Check 8
    path.closePath();
    pt.setLocation(12, 13.4f);
    harness.check(pt.equals(path.getCurrentPoint()));

    // Check 9
    path.moveTo(50, 51);
    pt.setLocation(50, 51);
    harness.check(pt.equals(path.getCurrentPoint()));

    // Check 10
    path.quadTo(52, 53, 54, 55);
    pt.setLocation(54, 55);
    harness.check(pt.equals(path.getCurrentPoint()));

    // Check 11
    path.closePath();
    pt.setLocation(50, 51);
    harness.check(pt.equals(path.getCurrentPoint()));

    // Check 12
    path.reset();
    harness.check(path.getCurrentPoint() == null);
  }
}
