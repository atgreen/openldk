/* getDividerLocation.java -- Checks BasicSplitPaneUI.getDividerLocation()
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

package gnu.testlet.javax.swing.plaf.basic.BasicSplitPaneUI;

import java.awt.Component;

import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.plaf.basic.BasicSplitPaneUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks BasicSplitPaneUI.getDividerLocation().
 */
public class getDividerLocation implements Testlet
{

  /**
   * Checks that BasicSplitPaneUI.getDividerLocation() simply
   * returns the real location of the divider component, regardless
   * of the JSplitPaneUI setting.
   */
  public void test(TestHarness harness)
  {
    // Check horizontal.
    JSplitPane sp = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                                   new JPanel(), new JPanel());
    BasicSplitPaneUI ui = (BasicSplitPaneUI) sp.getUI();
    Component divider = sp.getComponent(2);
    divider.setLocation(1234, 5678);
    harness.check(ui.getDividerLocation(sp), 1234);

    // Check vertical.
    sp = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
                                   new JPanel(), new JPanel());
    ui = (BasicSplitPaneUI) sp.getUI();
    divider = sp.getComponent(2);
    divider.setLocation(1234, 5678);
    harness.check(ui.getDividerLocation(sp), 5678);
  }

}
