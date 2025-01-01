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

package gnu.testlet.java.awt.geom.Rectangle2D;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.Rectangle;
import java.awt.geom.AffineTransform;
import java.awt.geom.PathIterator;
import java.awt.geom.Rectangle2D;

import java.util.NoSuchElementException;

/**
 * Checks that Rectangle2D.getPathIterator works correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class getPathIterator
  implements Testlet
{
  private static int M = PathIterator.SEG_MOVETO;
  private static int L = PathIterator.SEG_LINETO;
  private static int C = PathIterator.SEG_CLOSE;

  private int[] segs = { M, L, L, L, L, C };
  private double[] c1 = { 10, 20, 40, 20, 40, 60, 10, 60, 10, 20 };
  private double[] c2 = { 75, 106, 105, 166, 225, 326, 195, 266, 75, 106};
  
  public void test(TestHarness harness)
  {
    Rectangle2D ri, rf, rd;

    AffineTransform tx;
    PathIterator iter;

    ri = new Rectangle(10, 20, 30, 40);
    rf = new Rectangle.Float(10, 20, 30, 40);
    rd = new Rectangle.Double(10, 20, 30, 40);

    tx = new AffineTransform(1,2,3,4,5,6);

    harness.checkPoint("Rectangle.getPathIterator(null)");
    checkPathIterator(harness, ri.getPathIterator(null), segs, c1);

    harness.checkPoint("Rectangle2D.Float.getPathIterator(null)");
    checkPathIterator(harness, rf.getPathIterator(null), segs, c1);

    harness.checkPoint("Rectangle2D.Double.getPathIterator(null)");
    checkPathIterator(harness, rd.getPathIterator(null), segs, c1);



    harness.checkPoint("Rectangle.getPathIterator(null, 0.4)");
    checkPathIterator(harness, ri.getPathIterator(null, 0.4), segs, c1);

    harness.checkPoint("Rectangle2D.Float.getPathIterator(null, 0.4)");
    checkPathIterator(harness, rf.getPathIterator(null, 0.4), segs, c1);

    harness.checkPoint("Rectangle2D.Float.getPathIterator(null, 0.4)");
    checkPathIterator(harness, rd.getPathIterator(null, 0.4), segs, c1);



    harness.checkPoint("Rectangle.getPathIterator(tx)");
    checkPathIterator(harness, ri.getPathIterator(tx), segs, c2);

    harness.checkPoint("Rectangle2D.Float.getPathIterator(tx)");
    checkPathIterator(harness, rf.getPathIterator(tx), segs, c2);

    harness.checkPoint("Rectangle2D.Double.getPathIterator(tx)");
    checkPathIterator(harness, rd.getPathIterator(tx), segs, c2);



    harness.checkPoint("Rectangle.getPathIterator(tx, 1)");
    checkPathIterator(harness, ri.getPathIterator(tx, 1), segs, c2);

    harness.checkPoint("Rectangle2D.Float.getPathIterator(tx, 1)");
    checkPathIterator(harness, rf.getPathIterator(tx, 1), segs, c2);

    harness.checkPoint("Rectangle2D.Double.getPathIterator(tx, 1)");
    checkPathIterator(harness, rd.getPathIterator(tx, 1), segs, c2);
  }


  private static void checkPathIterator(TestHarness harness,
                                        PathIterator iter,
                                        int[] segs, double[] d)
  {
    int i = 0, j = 0;
    int segD, segF;
    double[] dc = new double[2];
    float[] fc = new float[2];
    boolean gotRightException;

    harness.check(iter.getWindingRule(), PathIterator.WIND_NON_ZERO);
    while (!iter.isDone())
    {
      segD = iter.currentSegment(dc);
      segF = iter.currentSegment(fc);
      harness.check(segD, segs[i]);
      harness.check(segF, segs[i]);
      switch (segD)
      {
      case PathIterator.SEG_MOVETO:
      case PathIterator.SEG_LINETO:
        harness.check(dc[0], d[j++]);
        harness.check(dc[1], d[j++]);
        harness.check(fc[0], (float) dc[0]);
        harness.check(fc[1], (float) dc[1]);
        break;
      }
      iter.next();
      i++;
    }

    harness.check(i, segs.length);

    gotRightException = false;
    try
    {
      iter.currentSegment(dc);
    }
    catch (NoSuchElementException ex)
    {
      gotRightException = true;
    }
    harness.check(gotRightException);

    /* Check that no exception is thrown here. */
    iter.next();
    harness.check(true);
  }

}
