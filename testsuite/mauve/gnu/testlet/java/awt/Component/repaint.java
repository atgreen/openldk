/* repaint.java -- Tests repaint() methods in Component
   Copyright (C) 2006 Roman Kennke (kennke@aicas.com)
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

// Tags: JDK1.1

package gnu.testlet.java.awt.Component;

import java.awt.Component;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests how the repaint() methods call each other.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class repaint implements Testlet
{
  /**
   * Indicate if one of the repaint methods got called.
   */
  boolean repaint1Called; // The no-arg version.
  boolean repaint2Called; // The repaint(long) version.
  boolean repaint3Called; // The repaint(int,int,int,int) version.
  boolean repaint4Called; // The repaint(long,int,int,int,int) version.
  
  /**
   * Overridden to check which repaint() method gets called when.
   */
  class TestComponent extends Component
  {
    public void repaint()
    {
      super.repaint();
      repaint1Called = true;
    }
    public void repaint(long tm)
    {
      super.repaint(tm);
      repaint2Called = true;
    }
    public void repaint(int x, int y, int w, int h)
    {
      super.repaint(x, y, w, h);
      repaint3Called = true;
    }
    public void repaint(long tm, int x, int y, int w, int h)
    {
      super.repaint(tm, x, y, w, h);
      repaint4Called = true;
    }
  }

  /**
   * The entry point into this test.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testNotShowing(harness);
  }

  /**
   * Tests how the repaint() methods call each other when the component is not
   * showing.
   *
   * @param h the test harness to use
   */
  private void testNotShowing(TestHarness h)
  {
    h.checkPoint("testNotShowing");
    TestComponent c = new TestComponent();
    // The component must not be showing at this point.
    h.check(!c.isShowing());

    repaint1Called = false;
    repaint2Called = false;
    repaint3Called = false;
    repaint4Called = false;
    c.repaint();
    h.check(repaint1Called);
    h.check(! repaint2Called);
    h.check(! repaint3Called);
    h.check(repaint4Called);

    repaint1Called = false;
    repaint2Called = false;
    repaint3Called = false;
    repaint4Called = false;
    c.repaint(100);
    h.check(! repaint1Called);
    h.check(repaint2Called);
    h.check(! repaint3Called);
    h.check(repaint4Called);

    repaint1Called = false;
    repaint2Called = false;
    repaint3Called = false;
    repaint4Called = false;
    c.repaint(0, 0, 1, 2);
    h.check(! repaint1Called);
    h.check(! repaint2Called);
    h.check(repaint3Called);
    h.check(repaint4Called);

    repaint1Called = false;
    repaint2Called = false;
    repaint3Called = false;
    repaint4Called = false;
    c.repaint(100, 0, 0, 1, 2);
    h.check(! repaint1Called);
    h.check(! repaint2Called);
    h.check(! repaint3Called);
    h.check(repaint4Called);
  }
}
