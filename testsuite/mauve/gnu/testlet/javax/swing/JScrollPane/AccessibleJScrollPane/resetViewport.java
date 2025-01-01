// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

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

package gnu.testlet.javax.swing.JScrollPane.AccessibleJScrollPane;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Point;
import java.beans.PropertyChangeEvent;
import javax.accessibility.AccessibleContext;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JViewport;

/**
 * Tests if the resetViewport method of AccessibleJViewport works correctly.
 */
public class resetViewport implements Testlet
{

  static boolean resetViewPortCalled;

  static class TestScrollPane extends JScrollPane
  {
    class TestAccessibleScrollPane extends AccessibleJScrollPane
    {
      public void resetViewPort()
      {
        resetViewPortCalled = true;
        super.resetViewPort();
      }
    }
    public AccessibleContext getAccessibleContext()
    {
      if (accessibleContext == null)
        accessibleContext = new TestAccessibleScrollPane();
      return accessibleContext;
    }
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    test1(harness);
    test2(harness);
    test3(harness);
  }

  /**
   * This test shows that resetViewPort is not used to reset the viewport
   * to the position (0, 0).
   *
   * @param harness the test harness to use
   */
  void test1(TestHarness harness)
  {
    JScrollPane sp = new TestScrollPane();
    JViewport vp = sp.getViewport();
    vp.setView(new JLabel("Hello World"));
    TestScrollPane.TestAccessibleScrollPane asp =
      (TestScrollPane.TestAccessibleScrollPane) sp.getAccessibleContext();
    vp.setViewPosition(new Point(10, 10));
    harness.check(vp.getViewPosition().equals(new Point(0, 0)), false);
    asp.resetViewPort();
    harness.check(vp.getViewPosition().equals(new Point(0, 0)), false);
  }

  /**
   * This test shows that resetViewPort is not called in response to a
   * property change of 'viewport' in the scrollpane.
   *
   * @param harness the test harness to use
   */
  void test2(TestHarness harness)
  {
    JScrollPane sp = new TestScrollPane();
    TestScrollPane.TestAccessibleScrollPane asp =
      (TestScrollPane.TestAccessibleScrollPane) sp.getAccessibleContext();
    resetViewPortCalled = false;
    asp.propertyChange(new PropertyChangeEvent(sp, "viewport",
                                               sp.getViewport(),
                                               new JViewport()));
    harness.check(resetViewPortCalled, false);
  }

  void test3(TestHarness harness)
  {
    JScrollPane sp = new TestScrollPane();
    TestScrollPane.TestAccessibleScrollPane asp =
      (TestScrollPane.TestAccessibleScrollPane) sp.getAccessibleContext();
    resetViewPortCalled = false;
    sp.setViewport(new JViewport());
    harness.check(resetViewPortCalled, true);
  }
}
