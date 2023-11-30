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
import java.awt.geom.IllegalPathStateException;
import java.awt.geom.Line2D;
import java.awt.geom.PathIterator;


/**
 * Checks whether the GeneralPath.append(PathIterator,boolean) methods
 * works correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class append_PathIterator
  implements Testlet
{
  public void test(TestHarness harness)
  {
    test_notConnecting(harness);
    test_connecting(harness);
    test_empty(harness);
  }


  /**
   * Appends a path iterator to an empty GeneralPath without
   * connecting the two together.
   */
  public void test_notConnecting(TestHarness harness)
  {
    harness.checkPoint("notConnecting");

    PathIterator piter, resPiter;
    GeneralPath path;
    double[] c = new double[6];

    piter = new PathIterator()
    {
      int i = 0;

      public int getWindingRule()
      {
        return PathIterator.WIND_EVEN_ODD;
      }

      public boolean isDone()
      {
        // System.out.println("isDone(), i=" + i);
        return i >= 7;
      }

      public void next()
      {
        ++i;
      }

      public int currentSegment(float[] c)
      {
        double[] d = new double[c.length];
        int r = currentSegment(d);
        for (int i = 0; i < d.length; i++)
          c[i] = (float) d[i];
        return r;
      }

      public int currentSegment(double[] c)
      {
        switch (i)
        {
        case 0:
          c[0] = 10; c[1] = 11;
          return PathIterator.SEG_MOVETO;

        case 1:
          c[0] = 20; c[1] = 21;
          return PathIterator.SEG_LINETO;

        case 2:
          c[0] = 30; c[1] = 31; c[2] = 32; c[3] = 33;
          return PathIterator.SEG_QUADTO;

        case 3:
          c[0] = 40; c[1] = 41; c[2] = 42; c[3] = 43;
          c[4] = 44; c[5] = 45;
          return PathIterator.SEG_CUBICTO;

        case 4:
          return PathIterator.SEG_CLOSE;

        case 5:
          c[0] = 50; c[1] = 51;
          return PathIterator.SEG_MOVETO;

        case 6:
          c[0] = 60; c[1] = 61;
          return PathIterator.SEG_LINETO;

        default:
          throw new IllegalPathStateException();
        }
      }
    };


    path = new GeneralPath(GeneralPath.WIND_NON_ZERO);
    path.append(piter, false);
    
    harness.check(path.getWindingRule(),      // 1
                  GeneralPath.WIND_NON_ZERO);

    resPiter = path.getPathIterator(null);
    harness.check(!resPiter.isDone());        // 2

    harness.check(resPiter.currentSegment(c), // 3
                  PathIterator.SEG_MOVETO);
    harness.check(c[0], 10);                  // 4
    harness.check(c[1], 11);                  // 5

    resPiter.next();
    harness.check(!resPiter.isDone());        // 6
    harness.check(resPiter.currentSegment(c), // 7
                  PathIterator.SEG_LINETO);
    harness.check(c[0], 20);                  // 8
    harness.check(c[1], 21);                  // 9

    resPiter.next();
    harness.check(!resPiter.isDone());        // 10
    harness.check(resPiter.currentSegment(c),
                  PathIterator.SEG_QUADTO);   // 11
    harness.check(c[0], 30);                  // 12
    harness.check(c[1], 31);                  // 13
    harness.check(c[2], 32);                  // 14
    harness.check(c[3], 33);                  // 15

    resPiter.next();
    harness.check(!resPiter.isDone());        // 16
    harness.check(resPiter.currentSegment(c),
                  PathIterator.SEG_CUBICTO);  // 17
    harness.check(c[0], 40);                  // 18
    harness.check(c[1], 41);                  // 19
    harness.check(c[2], 42);                  // 20
    harness.check(c[3], 43);                  // 21
    harness.check(c[4], 44);                  // 22
    harness.check(c[5], 45);                  // 23

    resPiter.next();
    harness.check(!resPiter.isDone());        // 24
    harness.check(resPiter.currentSegment(c), // 25
                  PathIterator.SEG_CLOSE);

    resPiter.next();
    harness.check(!resPiter.isDone());        // 26
    harness.check(resPiter.currentSegment(c), // 27
                  PathIterator.SEG_MOVETO);
    harness.check(c[0], 50);                  // 28
    harness.check(c[1], 51);                  // 29

    resPiter.next();
    harness.check(!resPiter.isDone());        // 30
    harness.check(resPiter.currentSegment(c), // 31
                  PathIterator.SEG_LINETO);
    harness.check(c[0], 60);                  // 32
    harness.check(c[1], 61);                  // 33

    resPiter.next();
    harness.check(resPiter.isDone());         // 34
    harness.check(piter.isDone());            // 35
  }


  /**
   * Appends a PathIterator to a non-empty path, connecting the two
   * together with a SEG_LINETO segment.
   */
  public void test_connecting(TestHarness harness)
  {
    GeneralPath path;
    PathIterator pit;
    float[] c = new float[6];

    harness.checkPoint("connecting");
    path = new GeneralPath();
    path.moveTo(10, 11);
    path.append(new Line2D.Double(20, 21, 30, 31).getPathIterator(null),
                /* connecting */ true);
    pit = path.getPathIterator(null);
    
    harness.check(!pit.isDone());             // 1
    harness.check(pit.currentSegment(c),      // 2
                  PathIterator.SEG_MOVETO);
    harness.check(c[0], 10);                  // 3
    harness.check(c[1], 11);                  // 4
    pit.next();
    harness.check(!pit.isDone());             // 5
    harness.check(pit.currentSegment(c),      // 6
                  PathIterator.SEG_LINETO);
    harness.check(c[0], 20);                  // 7
    harness.check(c[1], 21);                  // 8
    pit.next();
    harness.check(!pit.isDone());             // 9
    harness.check(pit.currentSegment(c),      // 10
                  PathIterator.SEG_LINETO);
    harness.check(c[0], 30);                  // 11
    harness.check(c[1], 31);                  // 12
    pit.next();
    harness.check(pit.isDone());              // 13
  }


  /**
   * Appends an empty path iterator to an empty GeneralPath.
   */
  public void test_empty(TestHarness harness)
  {
    PathIterator pit, respit;
    GeneralPath path;

    harness.checkPoint("empty");
    path = new GeneralPath();

    // Check 1
    path.append(new EmptyPathIterator(), false);
    harness.check(path.getPathIterator(null).isDone());

    // Check 2
    path.append(new EmptyPathIterator(), true);
    harness.check(path.getPathIterator(null).isDone());
  }



  /**
   * A path iterator for an empty path.
   */
  private static class EmptyPathIterator
    implements PathIterator
  {
    public int getWindingRule()
    {
      return PathIterator.WIND_EVEN_ODD;
    }

    public boolean isDone()
    {
      return true;
    }

    public void next()
    {
      throw new IllegalPathStateException();
    }

    public int currentSegment(float[] c)
    {
      throw new IllegalPathStateException();
    }

    public int currentSegment(double[] c)
    {
      throw new IllegalPathStateException();
    }    
  }
}
