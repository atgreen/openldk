/* layoutContainer.java -- Checks the layout manager layoutContainer()
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

package gnu.testlet.javax.swing.plaf.basic.BasicSplitPaneUI.BasicHorizontalLayoutManager;

import java.awt.Dimension;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.plaf.basic.BasicSplitPaneUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class layoutContainer implements Testlet
{

  public void test(TestHarness harness)
  {
    testMinimumSize(harness);
  }

  /**
   * Tests that the layout manager honors the minimum size of the
   * components.
   *
   * @param h the test harness
   */
  private void testMinimumSize(TestHarness h)
  {
    // Check without calling setDividerLocation().
    JPanel c1 = new JPanel();
    c1.setMinimumSize(new Dimension(100, 100));
    JPanel c2 = new JPanel();
    JSplitPane sp = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, c1, c2);
    sp.setSize(200, 200);
    h.check(sp.getLayout()
            instanceof BasicSplitPaneUI.BasicHorizontalLayoutManager);
    sp.getLayout().layoutContainer(sp);
    h.check(c1.getWidth(), 100);

    // Check with calling setDividerLocation().
    c1 = new JPanel();
    c1.setMinimumSize(new Dimension(100, 100));
    c2 = new JPanel();
    sp = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, c1, c2);
    sp.setDividerLocation(0);
    sp.setSize(200, 200);
    h.check(sp.getLayout()
            instanceof BasicSplitPaneUI.BasicHorizontalLayoutManager);
    sp.getLayout().layoutContainer(sp);
    h.check(c1.getWidth(), 0);
  }
}
