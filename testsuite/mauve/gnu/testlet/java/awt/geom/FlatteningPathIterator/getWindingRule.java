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

import java.awt.geom.FlatteningPathIterator;
import java.awt.geom.PathIterator;


/**
 * Checks whether FlatteningPathIterator.getFlatness works.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class getWindingRule
  implements Testlet
{
  public void test(TestHarness harness)
  {
    FlatteningPathIterator fpi;

    // Check 1
    fpi = new FlatteningPathIterator(
      new TestPathIterator(PathIterator.WIND_EVEN_ODD), 2.0);
    harness.check(fpi.getWindingRule(), PathIterator.WIND_EVEN_ODD);

    // Check 2
    fpi = new FlatteningPathIterator(
      new TestPathIterator(PathIterator.WIND_NON_ZERO), 23.0);
    harness.check(fpi.getWindingRule(), PathIterator.WIND_NON_ZERO);
  }


  private static class TestPathIterator
    implements PathIterator
  {
    int rule;

    public TestPathIterator(int rule)
    {
      this.rule = rule;
    }

    public int getWindingRule()
    {
      return rule;
    }

    public boolean isDone()
    {
      return true;
    }

    public int currentSegment(float[] f)
    {
      throw new IllegalStateException();
    }

    public int currentSegment(double[] d)
    {
      throw new IllegalStateException();
    }

    public void next()
    {
      throw new IllegalStateException();
    }
  }
}
