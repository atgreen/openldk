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

import java.awt.geom.PathIterator;
import java.awt.geom.Rectangle2D;

/**
 * Checks that the GeneralPath constructors works correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class GeneralPath
  implements Testlet
{
  public void test(TestHarness harness)
  {
    testZeroArgConstructor(harness);
    testIntArgConstructor(harness);
    testIntIntArgConstructor(harness);
    testShapeArgConstructor(harness);
  }


  /**
   * Checks whether the zero-argument constructor works.
   */
  public void testZeroArgConstructor(TestHarness harness)
  {
    java.awt.geom.GeneralPath path;

    harness.checkPoint("GeneralPath()");

    // Check 1
    path = new java.awt.geom.GeneralPath();
    harness.check(path.getWindingRule(),
                  java.awt.geom.GeneralPath.WIND_NON_ZERO);
  }


  /**
   * Checks whether the one-argument constructor taking an integer
   * works.
   */
  public void testIntArgConstructor(TestHarness harness)
  {
    java.awt.geom.GeneralPath path;
    Throwable caught;

    harness.checkPoint("GeneralPath(int)");

    // Check 1
    path = new java.awt.geom.GeneralPath(
      java.awt.geom.GeneralPath.WIND_NON_ZERO);
    harness.check(path.getWindingRule(),
                  java.awt.geom.GeneralPath.WIND_NON_ZERO);

    // Check 2
    path = new java.awt.geom.GeneralPath(
      java.awt.geom.GeneralPath.WIND_EVEN_ODD);
    harness.check(path.getWindingRule(),
                  java.awt.geom.GeneralPath.WIND_EVEN_ODD);

    // Check 3
    caught = null;
    try
    {
      new java.awt.geom.GeneralPath(23);
    }
    catch (Exception ex)
    {
      caught = ex;
    }
    harness.check(caught instanceof IllegalArgumentException);
  }


  /**
   * Checks whether the two-argument constructor taking two integers
   * works.
   */
  public void testIntIntArgConstructor(TestHarness harness)
  {
    java.awt.geom.GeneralPath path;
    Throwable caught;

    harness.checkPoint("GeneralPath(int,int)");

    // Check 1
    path = new java.awt.geom.GeneralPath(
      java.awt.geom.GeneralPath.WIND_NON_ZERO, 12);
    harness.check(path.getWindingRule(),
                  java.awt.geom.GeneralPath.WIND_NON_ZERO);

    // Check 2
    path = new java.awt.geom.GeneralPath(
      java.awt.geom.GeneralPath.WIND_EVEN_ODD, 12);
    harness.check(path.getWindingRule(),
                  java.awt.geom.GeneralPath.WIND_EVEN_ODD);

    // Check 3
    caught = null;
    try
    {
      new java.awt.geom.GeneralPath(23, 12);
    }
    catch (Exception ex)
    {
      caught = ex;
    }
    harness.check(caught instanceof IllegalArgumentException);
  }


  /**
   * Checks whether the one-argument constructor taking a Shape
   * works.
   */
  public void testShapeArgConstructor(TestHarness harness)
  {
    java.awt.geom.GeneralPath path;
    Throwable caught;
    PathIterator piter;
    double[] c = new double[6];

    harness.checkPoint("GeneralPath(Shape)");

    // Check 1
    caught = null;
    try
    {
      path = new java.awt.geom.GeneralPath(null);
    }
    catch (Exception ex)
    {
      caught = ex;
    }
    harness.check(caught instanceof NullPointerException);

    // Check 2 to 4
    path = new java.awt.geom.GeneralPath(new Rectangle2D.Double(10,20,30,40));
    piter = path.getPathIterator(null);
    harness.check(piter.currentSegment(c), PathIterator.SEG_MOVETO);
    harness.check(c[0], 10.0);
    harness.check(c[1], 20.0);

    // Check 5 to 7
    piter.next();
    harness.check(piter.currentSegment(c), PathIterator.SEG_LINETO);
    harness.check(c[0], 40.0);
    harness.check(c[1], 20.0);

    // Check 8 to 10
    piter.next();
    harness.check(piter.currentSegment(c), PathIterator.SEG_LINETO);
    harness.check(c[0], 40.0);
    harness.check(c[1], 60.0);

    // Check 11 to 13
    piter.next();
    harness.check(piter.currentSegment(c), PathIterator.SEG_LINETO);
    harness.check(c[0], 10.0);
    harness.check(c[1], 60.0);

    // Check 14 to 16
    piter.next();
    harness.check(piter.currentSegment(c), PathIterator.SEG_LINETO);
    harness.check(c[0], 10.0);
    harness.check(c[1], 20.0);

    // Check 17
    piter.next();
    harness.check(piter.currentSegment(c), PathIterator.SEG_CLOSE);

    // Check 18
    piter.next();
    harness.check(piter.isDone());

    // Check 19
    harness.check(piter.getWindingRule(), PathIterator.WIND_NON_ZERO);
  }
}

