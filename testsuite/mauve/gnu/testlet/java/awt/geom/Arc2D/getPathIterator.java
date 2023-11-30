//Tags: JDK1.2

//Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.geom.Arc2D;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.geom.Arc2D;
import java.awt.geom.PathIterator;

/**
 * Tests the getPathIterator() method of the {@link Arc2D} class.
 */
public class getPathIterator implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)       
  {
    double[] c = new double[6];
    Arc2D arc1 = new Arc2D.Double(1.0, 2.0, 3.0, 4.0, 0.0, 90.0, Arc2D.PIE);
    PathIterator iterator = arc1.getPathIterator(null);
    harness.check(!iterator.isDone());
    int segType = iterator.currentSegment(c);
    harness.check(segType == PathIterator.SEG_MOVETO);
    harness.check(c[0], 4.0);
    harness.check(c[1], 4.0);
    harness.check(!iterator.isDone());
    iterator.next();
    segType = iterator.currentSegment(c);
    harness.check(segType == PathIterator.SEG_CUBICTO);
    harness.check(c[0], 4.0);
    harness.check(c[1], 2.8954305003384135);
    harness.check(c[2], 3.3284271247461903);
    harness.check(c[3], 2.0);
    harness.check(c[4], 2.5);
    harness.check(c[5], 2.0);
    iterator.next();
    segType = iterator.currentSegment(c);
    harness.check(segType == PathIterator.SEG_LINETO);
    harness.check(c[0], 2.5);
    harness.check(c[1], 4.0);
    iterator.next();
    segType = iterator.currentSegment(c);
    harness.check(segType == PathIterator.SEG_CLOSE);
    iterator.next();
    harness.check(iterator.isDone());
  }

}
