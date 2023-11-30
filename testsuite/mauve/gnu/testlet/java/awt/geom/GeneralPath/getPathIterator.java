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

import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.awt.geom.PathIterator;


/**
 * Checks whether the GeneralPath.getPathIterator method works
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


  /**
   * Checks whether the getIterator method works when a null
   * transform is being passed.
   */
  private void test_untransformed(TestHarness h)
  {
    GeneralPath path;
    PathIterator pit;
    float[] f = new float[6];
    double[] d = new double[6];

    h.checkPoint("untransformed");
    path = new GeneralPath(GeneralPath.WIND_EVEN_ODD);
    path.moveTo(10, 11);
    path.lineTo(20, 21);
    path.closePath();
    path.moveTo(30, 31);
    path.quadTo(40, 41, 42, 43);
    path.curveTo(50, 51, 52, 53, 54, 55);
    
    pit = path.getPathIterator(null);
    h.check(pit.getWindingRule(), PathIterator.WIND_EVEN_ODD); // 1
    h.check(!pit.isDone());                                    // 2
    h.check(pit.currentSegment(f), PathIterator.SEG_MOVETO);   // 3
    h.check(f[0], 10);                                         // 4
    h.check(f[1], 11);                                         // 5
    h.check(pit.currentSegment(d), PathIterator.SEG_MOVETO);   // 6
    h.check(d[0], 10);                                         // 7
    h.check(d[1], 11);                                         // 8
    pit.next();
    h.check(!pit.isDone());                                    // 9
    h.check(pit.currentSegment(f), PathIterator.SEG_LINETO);   // 10
    h.check(f[0], 20);                                         // 11
    h.check(f[1], 21);                                         // 12
    h.check(pit.currentSegment(d), PathIterator.SEG_LINETO);   // 13
    h.check(d[0], 20);                                         // 14
    h.check(d[1], 21);                                         // 15
    pit.next();
    h.check(!pit.isDone());                                    // 16
    h.check(pit.currentSegment(f), PathIterator.SEG_CLOSE) ;   // 17
    h.check(pit.currentSegment(d), PathIterator.SEG_CLOSE) ;   // 18
    pit.next();
    h.check(!pit.isDone());                                    // 19
    h.check(pit.currentSegment(f), PathIterator.SEG_MOVETO);   // 20
    h.check(f[0], 30);                                         // 21
    h.check(f[1], 31);                                         // 22
    h.check(pit.currentSegment(d), PathIterator.SEG_MOVETO);   // 23
    h.check(d[0], 30);                                         // 24
    h.check(d[1], 31);                                         // 25
    pit.next();
    h.check(!pit.isDone());                                    // 26
    h.check(pit.currentSegment(f), PathIterator.SEG_QUADTO);   // 27
    h.check(f[0], 40);                                         // 28
    h.check(f[1], 41);                                         // 29
    h.check(f[2], 42);                                         // 30
    h.check(f[3], 43);                                         // 31
    h.check(pit.currentSegment(d), PathIterator.SEG_QUADTO);   // 32
    h.check(d[0], 40);                                         // 33
    h.check(d[1], 41);                                         // 34
    h.check(d[2], 42);                                         // 35
    h.check(d[3], 43);                                         // 36
    pit.next();
    h.check(!pit.isDone());                                    // 37
    h.check(pit.currentSegment(f), PathIterator.SEG_CUBICTO);  // 38
    h.check(f[0], 50);                                         // 39
    h.check(f[1], 51);                                         // 40
    h.check(f[2], 52);                                         // 41
    h.check(f[3], 53);                                         // 42
    h.check(f[4], 54);                                         // 43
    h.check(f[5], 55);                                         // 44
    h.check(pit.currentSegment(d), PathIterator.SEG_CUBICTO);  // 45
    h.check(d[0], 50);                                         // 46
    h.check(d[1], 51);                                         // 47
    h.check(d[2], 52);                                         // 48
    h.check(d[3], 53);                                         // 49
    h.check(d[4], 54);                                         // 50
    h.check(d[5], 55);                                         // 51
    pit.next();
    h.check(pit.isDone());                                     // 52
  }


  /**
   * Checks whether the getIterator method works when an affine
   * transformation is being passed.
   */
  private void test_transformed(TestHarness h)
  {
    GeneralPath path;
    PathIterator pit;
    AffineTransform tx;
    float[] f = new float[6];
    double[] d = new double[6];

    h.checkPoint("transformed");
    path = new GeneralPath(GeneralPath.WIND_NON_ZERO);
    path.moveTo(10, 11);
    path.lineTo(20, 21);
    path.closePath();
    path.moveTo(30, 31);
    path.quadTo(40, 41, 42, 43);
    path.curveTo(50, 51, 52, 53, 54, 55);
    
    tx = new AffineTransform();
    tx.translate(2, 3);
    tx.scale(10, 10);

    pit = path.getPathIterator(tx);
    h.check(pit.getWindingRule(), PathIterator.WIND_NON_ZERO); // 1
    h.check(!pit.isDone());                                    // 2
    h.check(pit.currentSegment(f), PathIterator.SEG_MOVETO);   // 3
    h.check(f[0], 102);                                        // 4
    h.check(f[1], 113);                                        // 5
    h.check(pit.currentSegment(d), PathIterator.SEG_MOVETO);   // 6
    h.check(d[0], 102);                                        // 7
    h.check(d[1], 113);                                        // 8
    pit.next();
    h.check(!pit.isDone());                                    // 9
    h.check(pit.currentSegment(f), PathIterator.SEG_LINETO);   // 10
    h.check(f[0], 202);                                        // 11
    h.check(f[1], 213);                                        // 12
    h.check(pit.currentSegment(d), PathIterator.SEG_LINETO);   // 13
    h.check(d[0], 202);                                        // 14
    h.check(d[1], 213);                                        // 15
    pit.next();
    h.check(!pit.isDone());                                    // 16
    h.check(pit.currentSegment(f), PathIterator.SEG_CLOSE) ;   // 17
    h.check(pit.currentSegment(d), PathIterator.SEG_CLOSE) ;   // 18
    pit.next();
    h.check(!pit.isDone());                                    // 19
    h.check(pit.currentSegment(f), PathIterator.SEG_MOVETO);   // 20
    h.check(f[0], 302);                                        // 21
    h.check(f[1], 313);                                        // 22
    h.check(pit.currentSegment(d), PathIterator.SEG_MOVETO);   // 23
    h.check(d[0], 302);                                        // 24
    h.check(d[1], 313);                                        // 25
    pit.next();
    h.check(!pit.isDone());                                    // 26
    h.check(pit.currentSegment(f), PathIterator.SEG_QUADTO);   // 27
    h.check(f[0], 402);                                        // 28
    h.check(f[1], 413);                                        // 29
    h.check(f[2], 422);                                        // 30
    h.check(f[3], 433);                                        // 31
    h.check(pit.currentSegment(d), PathIterator.SEG_QUADTO);   // 32
    h.check(d[0], 402);                                        // 33
    h.check(d[1], 413);                                        // 34
    h.check(d[2], 422);                                        // 35
    h.check(d[3], 433);                                        // 36
    pit.next();
    h.check(!pit.isDone());                                    // 37
    h.check(pit.currentSegment(f), PathIterator.SEG_CUBICTO);  // 38
    h.check(f[0], 502);                                        // 39
    h.check(f[1], 513);                                        // 40
    h.check(f[2], 522);                                        // 41
    h.check(f[3], 533);                                        // 42
    h.check(f[4], 542);                                        // 43
    h.check(f[5], 553);                                        // 44
    h.check(pit.currentSegment(d), PathIterator.SEG_CUBICTO);  // 45
    h.check(d[0], 502);                                        // 46
    h.check(d[1], 513);                                        // 47
    h.check(d[2], 522);                                        // 48
    h.check(d[3], 533);                                        // 49
    h.check(d[4], 542);                                        // 50
    h.check(d[5], 553);                                        // 51
    pit.next();
    h.check(pit.isDone());                                     // 52
  }
}
