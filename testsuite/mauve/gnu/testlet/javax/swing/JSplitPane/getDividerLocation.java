/* getDividerLocation.java -- Checks JSplitPane.getDividerLocation()
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

package gnu.testlet.javax.swing.JSplitPane;

import javax.swing.JSplitPane;
import javax.swing.plaf.basic.BasicSplitPaneUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks JSplitPane.getDividerLocation().
 */
public class getDividerLocation implements Testlet
{

  /**
   * Overrides BasicSplitPaneUI.getDividerLocation() for testing.
   */
  private class TestSplitPaneUI extends BasicSplitPaneUI
  {
    public int getDividerLocation(JSplitPane sp)
    {
      return 9876;
    }
  }

  /**
   * Entry point into the test.
   */
  public void test(TestHarness harness)
  {
    testCallUI(harness);
  }

  /**
   * Checks that getDividerLocation() does _not_ call into the UI, and
   * rather manages its property itself.
   *
   * @param h the test harness
   */
  private void testCallUI(TestHarness h)
  {
    JSplitPane sp = new JSplitPane();
    sp.setDividerLocation(1234);
    sp.setUI(new TestSplitPaneUI());
    h.check(sp.getDividerLocation(), 1234);
  }
}
