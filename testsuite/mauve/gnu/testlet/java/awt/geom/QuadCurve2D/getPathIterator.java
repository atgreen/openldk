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

import java.awt.geom.AffineTransform;
import java.awt.geom.PathIterator;
import java.awt.geom.QuadCurve2D;


/**
 * Checks whether the QuadCurve2D.getPathIterator method works
 * correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class getPathIterator
  implements Testlet
{
  public void test(TestHarness harness)
  {
    test_untransformed(harness);
    test_transformed(harness);
  }


  public void test_untransformed(TestHarness harness)
  {
    QuadCurve2D curve;
    PathIterator pit;
    double[] c = new double[6];

    harness.checkPoint("untransformed");
    curve = new QuadCurve2D.Double(1, 2, 3, 4, 5, 6);
    pit = curve.getPathIterator(null);

    harness.check(!pit.isDone());                                   // 1
    harness.check(pit.currentSegment(c), PathIterator.SEG_MOVETO);  // 2
    harness.check(c[0], 1.0);                                       // 3
    harness.check(c[1], 2.0);                                       // 4

    pit.next();
    harness.check(!pit.isDone());                                   // 5
    harness.check(pit.currentSegment(c), PathIterator.SEG_QUADTO);  // 6
    harness.check(c[0], 3.0);                                       // 7
    harness.check(c[1], 4.0);                                       // 8
    harness.check(c[2], 5.0);                                       // 9
    harness.check(c[3], 6.0);                                       // 10

    pit.next();
    harness.check(pit.isDone());                                    // 11
    harness.check(pit.getWindingRule(), PathIterator.WIND_NON_ZERO);// 12
  }


  public void test_transformed(TestHarness harness)
  {
    QuadCurve2D curve;
    PathIterator pit;
    AffineTransform tx;
    double[] c = new double[6];

    harness.checkPoint("transformed");
    tx = new AffineTransform();
    tx.scale(2, 3);
    tx.translate(1, -1);
    curve = new QuadCurve2D.Double(1, 2, 3, 4, 5, 6);
    pit = curve.getPathIterator(tx);

    harness.check(!pit.isDone());                                   // 1
    harness.check(pit.currentSegment(c), PathIterator.SEG_MOVETO);  // 2
    harness.check(c[0], 4.0);                                       // 3
    harness.check(c[1], 3.0);                                       // 4

    pit.next();
    harness.check(!pit.isDone());                                   // 5
    harness.check(pit.currentSegment(c), PathIterator.SEG_QUADTO);  // 6
    harness.check(c[0], 8.0);                                       // 7
    harness.check(c[1], 9.0);                                       // 8
    harness.check(c[2], 12.0);                                      // 9
    harness.check(c[3], 15.0);                                      // 10

    pit.next();
    harness.check(pit.isDone());                                    // 11

    harness.check(pit.getWindingRule(), PathIterator.WIND_NON_ZERO);// 12
  }
}
