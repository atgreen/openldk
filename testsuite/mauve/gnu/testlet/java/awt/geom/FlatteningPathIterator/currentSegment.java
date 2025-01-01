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

package gnu.testlet.java.awt.geom.FlatteningPathIterator;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.Shape;
import java.awt.geom.FlatteningPathIterator;
import java.awt.geom.GeneralPath;
import java.awt.geom.PathIterator;
import java.awt.geom.QuadCurve2D;
import java.awt.geom.CubicCurve2D;


/**
 * Checks whether FlatteningPathIterator.currentSegment works. This is
 * the real test for the flattening functionality.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class currentSegment
  implements Testlet
{
  public void test(TestHarness h)
  {
    test_emptyPath(h);
    test_quadCurve(h);
    test_cubicCurve(h);
  }


  /**
   * Flattens an empty path.
   */
  private void test_emptyPath(TestHarness h)
  {
    FlatteningPathIterator fpi;
    GeneralPath path;

    h.checkPoint("emptyPath");

    path = new GeneralPath();
    fpi = new FlatteningPathIterator(
     path.getPathIterator(null),
     /* closely follow the shape */ 1e-4,
     /* but without any recursion */ 0);
    h.check(fpi.isDone());
  }


  /**
   * Flattens quadratic curves with various flatnesses and recursion
   * limits.
   */
  public void test_quadCurve(TestHarness h)
  {
    h.checkPoint("quadCurve-A");
    test_quadCurve(h, /* flatness */ 0.0, /* level */ 0,
                   10,-10, 20, -1234, 400, 800, new double[]
    {
      /* Check 1 */ PathIterator.SEG_MOVETO, 10.0, -10.0,
      /* Check 2 */ PathIterator.SEG_LINETO, 400.0, 800.0
      /* Check 3: PathIterator.isDone() */
    });

    h.checkPoint("quadCurve-B");
    test_quadCurve(h, /* flatness */ 0.0, /* level */ 1,
                   10,-10, 20, -1234, 400, 800, new double[]
    {
      /* Check 1 */ PathIterator.SEG_MOVETO, 10.0, -10.0,
      /* Check 2 */ PathIterator.SEG_LINETO, 112.5, -419.5,
      /* Check 3 */ PathIterator.SEG_LINETO, 400.0, 800.0,
      /* Check 4: PathIterator.isDone() */
    });

    h.checkPoint("quadCurve-C");
    test_quadCurve(h, /* flatness */ 0.0, /* level */ 2,
                   0, -0, 1, -1, 2, -2, new double[]
    {
      /* Check 1 */ PathIterator.SEG_MOVETO, 0.0, 0.0,
      /* Check 2 */ PathIterator.SEG_LINETO, 0.5, -0.5,
      /* Check 3 */ PathIterator.SEG_LINETO, 1.0, -1.0,
      /* Check 4 */ PathIterator.SEG_LINETO, 1.5, -1.5,
      /* Check 5 */ PathIterator.SEG_LINETO, 2.0, -2.0,
      /* Check 6: PathIterator.isDone() */
    });

    h.checkPoint("quadCurve-D");
    test_quadCurve(h, /* flatness */ 0.1, /* level */ 2,
                   0, -0, 1, -1, 2, -2, new double[]
    {
      /* Check 1 */ PathIterator.SEG_MOVETO, 0.0, 0.0,
      /* Check 2 */ PathIterator.SEG_LINETO, 2.0, -2.0,
      /* Check 3: PathIterator.isDone() */
    });

    h.checkPoint("quadCurve-E");
    // flatness of this curve: 2.3417; test with smaller flatness
    test_quadCurve(h, /* flatness */ 1, /* level */ 8,
                   10, -20, 3, 4, 5, 6, new double[]
    {
      /* Check 1 */ PathIterator.SEG_MOVETO, 10.0, -20.0,
      /* Check 2 */ PathIterator.SEG_LINETO, 5.25, -1.5,
      /* Check 3 */ PathIterator.SEG_LINETO, 4.5625, 3.625,
      /* Check 4 */ PathIterator.SEG_LINETO, 5.0, 6.0,
      /* Check 5: PathIterator.isDone() */
    });

    h.checkPoint("quadCurve-F");
    // flatness of this curve: 2.3417; test with larger flatness
    test_quadCurve(h, /* flatness */ 4, /* level */ 8,
                   10, -20, 3, 4, 5, 6, new double[]
    {
      /* Check 1 */ PathIterator.SEG_MOVETO, 10.0, -20.0,
      /* Check 2 */ PathIterator.SEG_LINETO, 5.0, 6.0,
      /* Check 3: PathIterator.isDone() */
    });

    h.checkPoint("quadCurve-G");
    // degenerated: one single point
    test_quadCurve(h, /* flatness */ 4, /* level */ 8,
                   2.3, 2.3, 2.3, 2.3, 2.3, 2.3, new double[]
    {
      /* Check 1 */ PathIterator.SEG_MOVETO, 2.3, 2.3,
      /* Check 2 */ PathIterator.SEG_LINETO, 2.3, 2.3,
      /* Check 3: PathIterator.isDone() */
    });
  }


  /**
   * Flattens a QuadCurve2D with the specified flatness and recursion
   * level.
   */
  private void test_quadCurve(TestHarness h, double flatness, int level,
                              double x1, double y1,
                              double cx, double cy,
                              double x2, double y2,
                              double[] data)
  {
    Shape curve;
    FlatteningPathIterator fpi;

    curve = new QuadCurve2D.Double(x1, y1, cx, cy, x2, y2);
    fpi = new FlatteningPathIterator(curve.getPathIterator(null),
                                     flatness, level);
    if (data == null)
      dump(fpi);
    else
      checkSegments(h, fpi, data);
  }


  /**
   * Flattens cubic curves with various flatnesses and recursion
   * limits.
   */
  public void test_cubicCurve(TestHarness h)
  {
    h.checkPoint("cubicCurve-A");
    test_cubicCurve(h, /* flatness */ 0.0, /* level */ 0,
                   10,-10, 20, -1234, 400, 800, 0, 1, new double[]
    {
      /* Check 1 */ PathIterator.SEG_MOVETO, 10.0, -10.0,
      /* Check 2 */ PathIterator.SEG_LINETO, 0.0, 1.0,
      /* Check 3: PathIterator.isDone() */
    });

    h.checkPoint("cubicCurve-B");
    test_cubicCurve(h, /* flatness */ 0.0, /* level */ 1,
                   10,-10, 20, -1234, 400, 800, 120, 10, new double[]
    {
      /* Check 1 */ PathIterator.SEG_MOVETO, 10.0, -10.0,
      /* Check 2 */ PathIterator.SEG_LINETO, 173.75, -162.75,
      /* Check 3 */ PathIterator.SEG_LINETO, 120.0, 10.0,
      /* Check 4: PathIterator.isDone() */
    });

    h.checkPoint("cubicCurve-C");
    test_cubicCurve(h, /* flatness */ 0.0, /* level */ 2,
                   0, -0, 1, -1, 2, -2, 3, -3, new double[]
    {
      /* Check 1 */ PathIterator.SEG_MOVETO, 0.0, 0.0,
      /* Check 2 */ PathIterator.SEG_LINETO, 0.75, -0.75,
      /* Check 3 */ PathIterator.SEG_LINETO, 1.5, -1.5,
      /* Check 4 */ PathIterator.SEG_LINETO, 2.25, -2.25,
      /* Check 5 */ PathIterator.SEG_LINETO, 3.0, -3.0,
      /* Check 6: PathIterator.isDone() */
    });

    h.checkPoint("cubicCurve-D");
    test_cubicCurve(h, /* flatness */ 0.1, /* level */ 2,
                    0, -0, 1, -1, 2, -2, 3, -3, new double[]
    {
      /* Check 1 */ PathIterator.SEG_MOVETO, 0.0, 0.0,
      /* Check 2 */ PathIterator.SEG_LINETO, 3.0, -3.0,
      /* Check 3: PathIterator.isDone() */
    });

    h.checkPoint("cubicCurve-E");
    // Flatness of this curve: 4.4034; of left subdivision: 0.8506;
    // of right subdivision: 1.281. Test with a flatness that is
    // larger than that of the whole curve.
    test_cubicCurve(h, /* flatness */ 5, /* level */ 8,
                    10, -20, 3, 4, 5, 6, 7, 8, new double[]
    {
      /* Check 1 */ PathIterator.SEG_MOVETO, 10.0, -20.0,
      /* Check 2 */ PathIterator.SEG_LINETO, 7.0, 8.0,
      /* Check 3: PathIterator.isDone() */
    });

    h.checkPoint("cubicCurve-F");
    // Flatness of this curve: 4.4034; of left subdivision: 0.8506;
    // of right subdivision: 1.281. Test with a flatness that is
    // larger than each subdivision, but smaller than the whole curve.
    test_cubicCurve(h, /* flatness */ 3, /* level */ 8,
                    10, -20, 3, 4, 5, 6, 7, 8, new double[]
    {
      /* Check 1 */ PathIterator.SEG_MOVETO, 10.0, -20.0,
      /* Check 2 */ PathIterator.SEG_LINETO, 5.125, 2.25,
      /* Check 3 */ PathIterator.SEG_LINETO, 7.0, 8.0,
      /* Check 4: PathIterator.isDone() */
    });

    h.checkPoint("cubicCurve-G");
    // Flatness of this curve: 4.4034; of left subdivision: 0.8506;
    // of right subdivision: 1.281. Test with a flatness that is
    // larger than the left, but smaller than the right subdivision.
    test_cubicCurve(h, /* flatness */ 1, /* level */ 8,
                    10, -20, 3, 4, 5, 6, 7, 8, new double[]
    {
      /* Check 1 */ PathIterator.SEG_MOVETO, 10.0, -20.0,
      /* Check 2 */ PathIterator.SEG_LINETO, 5.125, 2.25,
      /* Check 3 */ PathIterator.SEG_LINETO, 5.640625, 6.15625,
      /* Check 4 */ PathIterator.SEG_LINETO, 7.0, 8.0,
      /* Check 5: PathIterator.isDone() */
    });

    h.checkPoint("cubicCurve-H");
    // Flatness of this curve: 4.4034; of left subdivision: 0.8506;
    // of right subdivision: 1.281. Test with a flatness that is
    // smaller than each subdivision.
    test_cubicCurve(h, /* flatness */ 0.8, /* level */ 8,
                    10, -20, 3, 4, 5, 6, 7, 8, new double[]
    {
      /* Check 1 */ PathIterator.SEG_MOVETO, 10.0, -20.0,
      /* Check 2 */ PathIterator.SEG_LINETO, 6.296875, -5.78125,
      /* Check 3 */ PathIterator.SEG_LINETO, 5.125, 2.25,
      /* Check 4 */ PathIterator.SEG_LINETO, 5.640625, 6.15625,
      /* Check 5 */ PathIterator.SEG_LINETO, 7.0, 8.0,
      /* Check 6: PathIterator.isDone() */
    });


    h.checkPoint("cubicCurve-I");
    // degenerated: one single point
    test_cubicCurve(h, /* flatness */ 4, /* level */ 8,
                    2.3, 2.3, 2.3, 2.3, 2.3, 2.3, 2.3, 2.3, new double[]
    {
      /* Check 1 */ PathIterator.SEG_MOVETO, 2.3, 2.3,
      /* Check 2 */ PathIterator.SEG_LINETO, 2.3, 2.3,
      /* Check 3: PathIterator.isDone() */
    });
  }



  /**
   * Flattens a CubicCurve2D with the specified flatness and recursion
   * level.
   */
  private void test_cubicCurve(TestHarness h, double flatness, int level,
                               double x1, double y1,
                               double cx1, double cy1,
                               double cx2, double cy2,
                               double x2, double y2,
                               double[] data)
  {
    CubicCurve2D curve;
    FlatteningPathIterator fpi;

    curve = new CubicCurve2D.Double(x1, y1, cx1, cy1, cx2, cy2, x2, y2);
    fpi = new FlatteningPathIterator(curve.getPathIterator(null),
                                     flatness, level);
    if (data == null)
    {
      dump(fpi);
      /*
        CubicCurve2D l = new CubicCurve2D.Double();
        CubicCurve2D r = new CubicCurve2D.Double();
        curve.subdivide(l, r);
        System.out.println(curve.getFlatness());
        System.out.println(l.getFlatness());
        System.out.println(r.getFlatness());
      */
    }
    else
      checkSegments(h, fpi, data);
  }




  /**
   * Flattens a GeneralPath.
   */
  private void test_generalPath(TestHarness h)
  {
    h.checkPoint("generalPath");

    GeneralPath path;
    path = new GeneralPath();
    path.moveTo(1.1f, 1.2f);
    path.lineTo(2.1f, 2.2f);
    path.quadTo(3.1f, 3.2f, 4.1f, 4.2f);
    path.closePath();
    path.moveTo(5.1f, 5.2f);
    path.curveTo(6.1f, 6.2f, 7.1f, 7.2f, 8.1f, 8.2f);

  }


  private static final float EPSILON_F = 1e-6f;
  private static final double EPSILON = 1e-6;


  private void checkSegments(TestHarness h, PathIterator pit, double[] pt)
  {
    for (int i = 0; i < pt.length/3; i++)
    {
      checkSegment(h, pit, i, pt);
      pit.next();
    }
    h.check(pit.isDone());
  }


  private void checkSegment(TestHarness h, PathIterator pit, int seg,
                            double[] pt)
  {
    double[] d = new double[6];
    float[] f = new float[6];
    double x, y;
    int segType, expectedSegType;

    if (pit.isDone())
    {
      h.check(false);
      h.debug("path iterator is prematurely done");
      return;
    }

    expectedSegType = (int) pt[3 * seg];
    segType = pit.currentSegment(d);
    if (segType != expectedSegType)
    {
      h.debug("segment type mismatch (double[]): got "
              + getSegmentTypeName(segType)
              + ", expected " + getSegmentTypeName(expectedSegType));
      return;
    }

    segType = pit.currentSegment(f);
    if (segType != (int) pt[3 * seg])
    {
      h.check(segType, (int) pt[3 * seg]);
      h.debug("segment type mismatch (float[])");
      return;
    }

    if (segType == PathIterator.SEG_CLOSE)
    {
      h.check(true);
      return;
    }

    x = pt[3 * seg + 1];
    y = pt[3 * seg + 2];
    if ((Math.abs(d[0] - x) > EPSILON)
        || (Math.abs(d[1] - y) > EPSILON))
    {
      chkpt(h, d, 0, x, y);
      return;
    }

    chkpt(h, f, 0, (float) x, (float) y);
  }


  private void chkpt(TestHarness h, float[] f, int off, float x, float y)
  {
    if ((Math.abs(f[off] - x) > EPSILON_F)
        || (Math.abs(f[off+1] - y) > EPSILON_F))
    {
      h.check(false);
      h.debug("got (" + f[off] + ", " + f[off+1] + ") but expected ("
              + x + ", " + y + ")");
    }
    else
      h.check(true);
  }


  private void chkpt(TestHarness h, double[] f, int off, double x, double y)
  {
    if ((Math.abs(f[off] - x) > EPSILON)
        || (Math.abs(f[off+1] - y) > EPSILON))
    {
      h.check(false);
      h.debug("got (" + f[off] + ", " + f[off+1] + ") but expected ("
              + x + ", " + y + ")");
    }
    else
      h.check(true);
  }


  private static String getSegmentTypeName(int seg)
  {
    switch (seg)
    {
    case PathIterator.SEG_MOVETO:
      return "PathIterator.SEG_MOVETO";

    case PathIterator.SEG_LINETO:  
      return "PathIterator.SEG_LINETO";

    case PathIterator.SEG_QUADTO:
      return "PathIterator.SEG_QUADTO";
      
    case PathIterator.SEG_CUBICTO:
      return "PathIterator.SEG_CUBICTO";

    case PathIterator.SEG_CLOSE:
      return "PathIterator.SEG_CLOSE";

    default:
      throw new IllegalArgumentException();
    }
  }


  private void dump(PathIterator p)
  {
    double[] d = new double[6];
    int seg, i = 1;

    System.out.println("    {");
    while (!p.isDone())
    {
      seg = p.currentSegment(d);
      System.out.print("      /* Check " + (i++) + " */ ");
      System.out.print(getSegmentTypeName(seg));
      System.out.println(", " + d[0] + ", " + d[1] + ",");
      p.next();
    }
    System.out.println("      /* Check " + i
                       + ": PathIterator.isDone() */");
    System.out.println("    });");
  }
}
    
