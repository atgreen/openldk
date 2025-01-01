/* updateLayoutStateNeeded.java -- Checks the updateLayoutStateNeeded flag
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.plaf.basic.BasicListUI;

import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.plaf.basic.BasicListUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks when and how the updateLayoutStateNeeded flag is modified.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class updateLayoutStateNeeded implements Testlet
{

  /**
   * Overridden to make the updateLayoutStateNeeded field accessible.
   */
  private class TestListUI extends BasicListUI
  {
    public int getUpdateLayoutStateNeeded()
    {
      return updateLayoutStateNeeded;
    }
    public void maybeUpdateLayoutState()
    {
      super.maybeUpdateLayoutState();
    }
  }

  /**
   * The entry point of this test.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testModelDataChange(harness);
  }

  /**
   * Changes the model data and checks if the flag should be updated.
   *
   * @param h the test harness to use
   */
  private void testModelDataChange(TestHarness h)
  {
    JList l = new JList(new DefaultListModel());
    TestListUI ui = new TestListUI();
    l.setUI(ui);
    ui.maybeUpdateLayoutState();
    // The flag should be 0 after maybeUpdateLayoutState().
    h.check(ui.getUpdateLayoutStateNeeded(), 0);
    ((DefaultListModel) l.getModel()).addElement("test");
    // The flag should be 1 after the model data change.
    h.check(ui.getUpdateLayoutStateNeeded(), 1);
  }
}
