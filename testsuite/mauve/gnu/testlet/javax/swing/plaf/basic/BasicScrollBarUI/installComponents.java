// Tags: JDK1.5
// Uses: MyBasicScrollBarUI

// Copyright (C) 2006 Roman Kennke <kennke@aicas.com>

// This file is part of Mauve.

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

package gnu.testlet.javax.swing.plaf.basic.BasicScrollBarUI;

import javax.swing.JScrollBar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests if installComponents() works correctly. ATM we check if the fields
 * incrButton and decrButton are initialized within this method, since we had
 * a bug with this. There is certainly more that could be checked here.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class installComponents implements Testlet
{

  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testIncrButton(harness);
    testDecrButton(harness);
  }

  /**
   * Tests if the incrButton field is initialized in this method.
   *
   * @param harness the test harness to use
   */
  private void testIncrButton(TestHarness harness)
  {
    MyBasicScrollBarUI ui = new MyBasicScrollBarUI();
    ui.setScrollbar(new JScrollBar());
    harness.check(ui.getIncrButton(), null);
    ui.installComponents();
    harness.check(ui.getIncrButton() != null);
  }

  /**
   * Tests if the decrButton field is initialized in this method.
   *
   * @param harness the test harness to use
   */
  private void testDecrButton(TestHarness harness)
  {
    MyBasicScrollBarUI ui = new MyBasicScrollBarUI();
    ui.setScrollbar(new JScrollBar());
    harness.check(ui.getDecrButton(), null);
    ui.installComponents();
    harness.check(ui.getDecrButton() != null);
  }
}
