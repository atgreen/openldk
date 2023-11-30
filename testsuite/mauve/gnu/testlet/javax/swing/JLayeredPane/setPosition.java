/* setPosition.java -- Checks the setPosition method in JLayeredPane
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

// This is tagged as 1.5 because it checks behaviour that is only present
// since JDK1.5. In previous implementations the JLayeredPane had to
// add/remove components to the container when changing it's position. Starting
// with JDK1.5 this is no longer necessary because of
// Container.setComponentZOrder().
// Tags: JDK1.5

package gnu.testlet.javax.swing.JLayeredPane;

import java.awt.Component;

import javax.swing.JLayeredPane;
import javax.swing.JPanel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class setPosition implements Testlet
{

  /**
   * Indicates if addImpl() has been called.
   */
  boolean addImplCalled;

  /**
   * Overridden to be able to detect if addImpl should be called.
   */
  class TestLayeredPane extends JLayeredPane
  {
    protected void addImpl(Component comp, Object constraint, int index)
    {
      super.addImpl(comp, constraint, index);
      addImplCalled = true;
    }
  }

  /**
   * The entry point into this test.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testAddImpl(harness);
  }

  /**
   * This adds some components to the layered pane, and checks if a call
   * to setPosition is allowed to call addImpl.
   *
   * @param h the test harness to use
   */
  private void testAddImpl(TestHarness h)
  {
    TestLayeredPane l = new TestLayeredPane();
    JPanel p1 = new JPanel();
    JPanel p2 = new JPanel();
    JPanel p3 = new JPanel();
    l.add(p1);
    l.add(p2);
    l.add(p3);
    addImplCalled = false;
    l.setPosition(p2, 0);
    h.check(addImplCalled, false);
  }
}
