// Tags: JDK1.2 
// Uses: MyBasicButtonUI

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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


package gnu.testlet.javax.swing.plaf.basic.BasicButtonUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Some checks for the clearTextShiftOffset() method.
 */
public class clearTextShiftOffset implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    MyBasicButtonUI ui = new MyBasicButtonUI();
    harness.check(ui.getTextShiftOffset(), 0);
    ui.setDefaultTextShiftOffsetField(99);
    ui.setTextShiftOffset();
    harness.check(ui.getTextShiftOffset(), 99);
    ui.clearTextShiftOffset();
    harness.check(ui.getTextShiftOffset(), 0);
  }
  
}
