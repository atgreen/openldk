// Tags: JDK1.2

//Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.JRootPane.RootLayout;

import java.awt.Component;
import java.awt.Insets;
import java.awt.Rectangle;

import javax.swing.BorderFactory;
import javax.swing.JRootPane;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks the functionality of the JRootPane.RootLayout.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class layoutContainer implements Testlet
{

  // TODO: Implement a test that checks the layout in case we have a menubar
  // installed.

  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testWithBorder(harness);
  }

  /**
   * Tests if the RootLayout works correctly when a border is set on the
   * rootpane.
   *
   * @param harness
   */
  private void testWithBorder(TestHarness harness)
  {
    harness.checkPoint("withBorder");
    JRootPane rp = new JRootPane();
    rp.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
    rp.setSize(100, 100);
    rp.getLayout().layoutContainer(rp);
    Insets i = rp.getInsets();

    // Check the glass pane.
    Component gp = rp.getGlassPane();
    harness.check(gp.getBounds(),
                  new Rectangle(i.left, i.right,
                                rp.getWidth() - i.left - i.right,
                                rp.getHeight() - i.top - i.bottom));
    // Check the layered pane.
    Component lp = rp.getLayeredPane();
    harness.check(lp.getBounds(),
                  new Rectangle(i.left, i.right,
                                rp.getWidth() - i.left - i.right,
                                rp.getHeight() - i.top - i.bottom));

    // Check the content pane.
    Component cp = rp.getContentPane();
    harness.check(cp.getBounds(),
                  new Rectangle(0, 0, rp.getWidth() - i.left - i.right,
                                rp.getHeight() - i.top - i.bottom));
  }
}
