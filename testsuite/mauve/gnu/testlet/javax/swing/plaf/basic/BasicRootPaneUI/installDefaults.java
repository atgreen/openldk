/* installDefaults.java -- Tests BasicRootPane.installDefaults
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

package gnu.testlet.javax.swing.plaf.basic.BasicRootPaneUI;

import java.awt.Color;

import javax.swing.JRootPane;
import javax.swing.UIManager;
import javax.swing.plaf.basic.BasicRootPaneUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests the BasicRootPaneUI.installDefaults method.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class installDefaults implements Testlet
{

  /**
   * Overridden to make installDefaults visible.
   */
  class TestRootUI extends BasicRootPaneUI
  {
    public void installDefaults(JRootPane rp)
    {
      super.installDefaults(rp);
    }
  }

  /**
   * The entry point for this test.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testBackground(harness);
  }

  /**
   * Checks if the installDefaults() method should touch the background
   * of the JRootPane.
   *
   * @param h the test harness to use
   */
  private void testBackground(TestHarness h)
  {
    // We even give it a potential value to set.
    UIManager.put("RootPane.background", Color.RED);
    TestRootUI ui = new TestRootUI();
    JRootPane rp = new JRootPane();
    rp.setBackground(null);
    ui.installDefaults(rp);
    h.check(rp.getBackground(), null);
  }
}
