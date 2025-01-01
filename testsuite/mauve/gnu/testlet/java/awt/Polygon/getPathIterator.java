/* getPathIterator.java -- some checks for the getPathIterator() method in the
       Polygon class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: 1.2

package gnu.testlet.java.awt.Polygon;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Polygon;
import java.awt.geom.AffineTransform;
import java.awt.geom.PathIterator;

public class getPathIterator implements Testlet
{
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
  }
  
  public void test1(TestHarness harness)
  {
    harness.checkPoint("(AffineTransform)");
    Polygon p = new Polygon(new int[] {0, 1, 1, 0}, 
                            new int[] {0, 0, 1, 1}, 4);
    
    // test with no tranform...
    PathIterator pi = p.getPathIterator(null);
    harness.check(pi.getWindingRule(), PathIterator.WIND_EVEN_ODD);
    double[] coords = new double[6];
    int t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_MOVETO);
    harness.check(coords[0], 0.0);
    harness.check(coords[1], 0.0);
    harness.check(pi.isDone(), false);
    pi.next();
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_LINETO);
    harness.check(coords[0], 1.0);
    harness.check(coords[1], 0.0);
    harness.check(pi.isDone(), false);
    pi.next();
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_LINETO);
    harness.check(coords[0], 1.0);
    harness.check(coords[1], 1.0);
    harness.check(pi.isDone(), false);
    pi.next();
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_LINETO);
    harness.check(coords[0], 0.0);
    harness.check(coords[1], 1.0);
    harness.check(pi.isDone(), false);
    pi.next();
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_CLOSE);
    harness.check(pi.isDone(), false);
    pi.next();
    harness.check(pi.isDone(), true);

    // test with tranform...
    AffineTransform trans = AffineTransform.getTranslateInstance(10.0, 11.0);
    pi = p.getPathIterator(trans);
    harness.check(pi.getWindingRule(), PathIterator.WIND_EVEN_ODD);
    coords = new double[6];
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_MOVETO);
    harness.check(coords[0], 10.0);
    harness.check(coords[1], 11.0);
    harness.check(pi.isDone(), false);
    pi.next();
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_LINETO);
    harness.check(coords[0], 11.0);
    harness.check(coords[1], 11.0);
    harness.check(pi.isDone(), false);
    pi.next();
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_LINETO);
    harness.check(coords[0], 11.0);
    harness.check(coords[1], 12.0);
    harness.check(pi.isDone(), false);
    pi.next();
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_LINETO);
    harness.check(coords[0], 10.0);
    harness.check(coords[1], 12.0);
    harness.check(pi.isDone(), false);
    pi.next();
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_CLOSE);
    harness.check(pi.isDone(), false);
    pi.next();
    harness.check(pi.isDone(), true);
  
  }
  
  public void test2(TestHarness harness)
  {
    harness.checkPoint("(AffineTransform, double)");
    Polygon p = new Polygon(new int[] {0, 1, 1, 0}, 
                            new int[] {0, 0, 1, 1}, 4);
    
    // test with no tranform...
    PathIterator pi = p.getPathIterator(null, 1.0);
    harness.check(pi.getWindingRule(), PathIterator.WIND_EVEN_ODD);
    double[] coords = new double[6];
    int t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_MOVETO);
    harness.check(coords[0], 0.0);
    harness.check(coords[1], 0.0);
    harness.check(pi.isDone(), false);
    pi.next();
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_LINETO);
    harness.check(coords[0], 1.0);
    harness.check(coords[1], 0.0);
    harness.check(pi.isDone(), false);
    pi.next();
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_LINETO);
    harness.check(coords[0], 1.0);
    harness.check(coords[1], 1.0);
    harness.check(pi.isDone(), false);
    pi.next();
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_LINETO);
    harness.check(coords[0], 0.0);
    harness.check(coords[1], 1.0);
    harness.check(pi.isDone(), false);
    pi.next();
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_CLOSE);
    harness.check(pi.isDone(), false);
    pi.next();
    harness.check(pi.isDone(), true);

    // test with tranform...
    AffineTransform trans = AffineTransform.getTranslateInstance(10.0, 11.0);
    pi = p.getPathIterator(trans, 1.0);
    harness.check(pi.getWindingRule(), PathIterator.WIND_EVEN_ODD);
    coords = new double[6];
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_MOVETO);
    harness.check(coords[0], 10.0);
    harness.check(coords[1], 11.0);
    harness.check(pi.isDone(), false);
    pi.next();
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_LINETO);
    harness.check(coords[0], 11.0);
    harness.check(coords[1], 11.0);
    harness.check(pi.isDone(), false);
    pi.next();
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_LINETO);
    harness.check(coords[0], 11.0);
    harness.check(coords[1], 12.0);
    harness.check(pi.isDone(), false);
    pi.next();
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_LINETO);
    harness.check(coords[0], 10.0);
    harness.check(coords[1], 12.0);
    harness.check(pi.isDone(), false);
    pi.next();
    t = pi.currentSegment(coords);
    harness.check(t, PathIterator.SEG_CLOSE);
    harness.check(pi.isDone(), false);
    pi.next();
    harness.check(pi.isDone(), true);
  }
}
