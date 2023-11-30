/* getPreferredSize.java -- Tests the getPreferredSize() method
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

package gnu.testlet.java.awt.Container;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.LayoutManager2;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests the getPreferredSize method.
 * 
 * @author Roman Kennke (kennke@aicas.com)
 */
public class getPreferredSize implements Testlet
{

  /**
   * Indicates if invalideLayout has been called on the layout manager or
   * not.
   */
  boolean invalidateCalled;

  /**
   * A layout manager used for testing.
   * 
   * @author Roman Kennke (kennke@aicas.com)
   */
  class TestLM implements LayoutManager2
  {

    public void addLayoutComponent(Component component, Object constraints)
    {
      // Nothing to do here.
    }

    public Dimension maximumLayoutSize(Container target)
    {
      // Nothing to do here.
      return null;
    }

    public float getLayoutAlignmentX(Container target)
    {
      // Nothing to do here.
      return 0;
    }

    public float getLayoutAlignmentY(Container target)
    {
      // Nothing to do here.
      return 0;
    }

    public void invalidateLayout(Container target)
    {
      invalidateCalled = true;
    }

    public void addLayoutComponent(String name, Component component)
    {
      // Nothing to do here.
    }

    public void removeLayoutComponent(Component component)
    {
      // Nothing to do here.
    }

    public Dimension preferredLayoutSize(Container parent)
    {
      return new Dimension(100, 100);
    }

    public Dimension minimumLayoutSize(Container parent)
    {
      // Nothing to do here.
      return null;
    }

    public void layoutContainer(Container parent)
    {
      // Nothing to do here.
    }
    
  }

  /**
   * The entry point into the test.
   */
  public void test(TestHarness harness)
  {
    testInvalid(harness);
  }

  /**
   * Tests if the container should call invalidateLayout() before asking the
   * preferredSize from a layout manager. The background is that in theory
   * this could lead to wrong value for preferredSize, when a layout manager
   * caches the preferredSize.
   *
   * @param harness the test harness to use
   */
  private void testInvalid(TestHarness harness)
  {
    Container c = new Container();
    LayoutManager2 lm = new TestLM();
    c.setLayout(lm);
    c.invalidate();
    invalidateCalled = false;
    c.getPreferredSize();
    c.getPreferredSize();
    harness.check(invalidateCalled, false);
  }
}
