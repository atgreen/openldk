/* preferredLayoutSize.java -- tests the preferredLayoutSize() method
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

package gnu.testlet.javax.swing.JRootPane.RootLayout;

import java.awt.Dimension;
import java.awt.LayoutManager;

import javax.swing.JPanel;
import javax.swing.JRootPane;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks the preferredLayoutSize() method in the RootLayout.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class preferredLayoutSize implements Testlet
{

  /**
   * The entry point in the test.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testInvalidLayout(harness);
  }

  /**
   * Tests the preferredLayoutSize() method when the layout is in an invalid
   * state. In particular this tests if the preferredSize is cached or not.
   * This is inspired by a bug where a JRootPane is asked for it's
   * preferredSize (thus storing a cached value of it), then the contentPane
   * changes (and so the parameters for calculating the preferredSize) and then
   * it is asked again and then returns the incorrect cached value.
   *
   * @param h the test harness to use
   */
  private void testInvalidLayout(TestHarness h)
  {
    JRootPane rp = new JRootPane();
    LayoutManager l = rp.getLayout();
    JPanel p = new JPanel();
    p.setPreferredSize(new Dimension(100, 100));
    rp.setContentPane(p);
    h.check(l.preferredLayoutSize(rp), new Dimension(100, 100));
    p.setPreferredSize(new Dimension(200, 200));
    h.check(l.preferredLayoutSize(rp), new Dimension(200, 200));
  }
}
