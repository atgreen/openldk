// Tags: JDK1.2
// Uses: MyBasicScrollBarUI

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

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

import javax.swing.JButton;
import javax.swing.SwingConstants;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * This tests the functionality of the createDecreaseButton method. This
 * test shows that this method does not change the decrButton field and
 * delivers a new button on each call.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class createDecreaseButton implements Testlet
{

  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    MyBasicScrollBarUI ui = new MyBasicScrollBarUI();
    harness.check(ui.getDecrButton(), null);
    JButton b1 = ui.createDecreaseButton(SwingConstants.NORTH);
    // The method call must not touch the decrButton field.
    harness.check(ui.getDecrButton(), null);
    // The method delivers a new instance on each call.
    JButton b2 = ui.createDecreaseButton(SwingConstants.NORTH);
    harness.check(b1 != b2);
  }

}
