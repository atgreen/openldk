// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke  <kennke@aicas.com>

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.JComponent;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.LayoutManager;

import javax.swing.JComponent;
import javax.swing.plaf.ComponentUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Some tests for JComponent.getPreferredSize().
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class getPreferredSize implements Testlet
{

  /**
   * A JComponent to overrides setUI for test purposes.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  static class TestComponent extends JComponent
  {
    public void setUI(ComponentUI ui)
    {
      super.setUI(ui);
    }
  }

  /**
   * A UI class for test purposes.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  static class TestUI extends ComponentUI
  {
    public Dimension getPreferredSize(JComponent c)
    {
      return new Dimension(100, 100);
    }
  }

  /**
   * A layout manager for test purposes.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  static class TestLayout implements LayoutManager
  {

    public void addLayoutComponent(String name, Component component)
    {
      // TODO Auto-generated method stub
      
    }

    public void removeLayoutComponent(Component component)
    {
      // TODO Auto-generated method stub
      
    }

    public Dimension preferredLayoutSize(Container parent)
    {
      return new Dimension(200, 200);
    }

    public Dimension minimumLayoutSize(Container parent)
    {
      // TODO Auto-generated method stub
      return null;
    }

    public void layoutContainer(Container parent)
    {
      // TODO Auto-generated method stub
      
    }
    
  }

  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testPlain(harness);
    testWithUI(harness);
    testWithLayout(harness);
    testWithUIAndLayout(harness);
    testWithSet(harness);
    testWithSetAndUI(harness);
    testWithSetAndLayout(harness);
    testWithAll(harness);

    testSmallerThanMinSize(harness);
    testChangeValue(harness);
  }

  /**
   * A very basic test.
   *
   * @param h the test harness to use
   */
  private void testPlain(TestHarness h)
  {
    h.checkPoint("plain");
    TestComponent c = new TestComponent();
    Dimension d = c.getPreferredSize();
    h.check(d.width, 0);
    h.check(d.height, 0);
  }

  /**
   * Tests the preferredSize with a UI installed.
   *
   * @param h the test harness to use
   */
  private void testWithUI(TestHarness h)
  {
    h.checkPoint("withUI");
    TestComponent c = new TestComponent();
    c.setUI(new TestUI());
    Dimension d = c.getPreferredSize();
    h.check(d.width, 100);
    h.check(d.height, 100);
  }

  /**
   * Tests the preferredSize with a layout manager installed
   *
   * @param h the test harness to use
   */
  private void testWithLayout(TestHarness h)
  {
    h.checkPoint("withLayout");
    TestComponent c = new TestComponent();
    c.setLayout(new TestLayout());
    Dimension d = c.getPreferredSize();
    h.check(d.width, 200);
    h.check(d.height, 200);
  }

  /**
   * Tests the preferredSize with both a layout manager and a UI installed.
   *
   * @param h the test harness to use
   */
  private void testWithUIAndLayout(TestHarness h)
  {
    h.checkPoint("withUIAndLayout");
    TestComponent c = new TestComponent();
    c.setUI(new TestUI());
    c.setLayout(new TestLayout());
    c.invalidate();
    Dimension d = c.getPreferredSize();
    h.check(d.width, 100);
    h.check(d.height, 100);
  }

  /**
   * Tests the preferredSize when explicitly set.
   *
   * @param h the test harness to use
   */
  private void testWithSet(TestHarness h)
  {
    h.checkPoint("withSet");
    TestComponent c = new TestComponent();
    c.setPreferredSize(new Dimension(300, 300));
    Dimension d = c.getPreferredSize();
    h.check(d.width, 300);
    h.check(d.height, 300);
  }

  /**
   * Tests the preferredSize when explicitly set and with UI installed.
   *
   * @param h the test harness to use
   */
  private void testWithSetAndUI(TestHarness h)
  {
    h.checkPoint("withSetAndUI");
    TestComponent c = new TestComponent();
    c.setPreferredSize(new Dimension(300, 300));
    c.setUI(new TestUI());
    Dimension d = c.getPreferredSize();
    h.check(d.width, 300);
    h.check(d.height, 300);
  }

  /**
   * Tests the preferredSize when explicitly set and layout manager installed.
   *
   * @param h the test harness to use
   */
  private void testWithSetAndLayout(TestHarness h)
  {
    h.checkPoint("withSetAndLayout");
    TestComponent c = new TestComponent();
    c.setPreferredSize(new Dimension(300, 300));
    c.setLayout(new TestLayout());
    Dimension d = c.getPreferredSize();
    h.check(d.width, 300);
    h.check(d.height, 300);
  }
  
  /**
   * Tests the preferredSize when explicitly set, layout manager installed and
   * UI installed.
   *
   * @param h the test harness to use
   */
  private void testWithAll(TestHarness h)
  {
    h.checkPoint("withAll");
    TestComponent c = new TestComponent();
    c.setUI(new TestUI());
    c.setLayout(new TestLayout());
    c.setPreferredSize(new Dimension(300, 300));
    Dimension d = c.getPreferredSize();
    h.check(d.width, 300);
    h.check(d.height, 300);
  }

  /**
   * Tests if the preferredSize is allowed to be smaller than the minimumSize.
   * 
   * @param h the test harness to use
   */
  private void testSmallerThanMinSize(TestHarness h)
  {
    h.checkPoint("smallerThanMinSize");
    TestComponent c = new TestComponent();
    c.setMinimumSize(new Dimension(100, 100));
    c.setPreferredSize(new Dimension(50, 50));
    h.check(c.getPreferredSize(), new Dimension(50, 50));
  }

  /**
   * Tests if it is possible to change to actual setting or preferredSize by
   * changing the values of the returned Dimension object.
   *
   * @param h the test harness
   */
  private void testChangeValue(TestHarness h)
  {
    h.checkPoint("changeValue");
    TestComponent c = new TestComponent();
    c.setPreferredSize(new Dimension(100, 100));
    Dimension d = c.getPreferredSize();
    d.width = 200;
    d.height = 200;
    h.check(c.getPreferredSize(), new Dimension(100, 100));
  }
}
