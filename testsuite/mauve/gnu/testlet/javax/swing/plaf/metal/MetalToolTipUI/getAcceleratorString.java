// Tags: JDK1.2

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

package gnu.testlet.javax.swing.plaf.metal.MetalToolTipUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.event.KeyEvent;

import javax.swing.JButton;
import javax.swing.JToolTip;
import javax.swing.plaf.metal.MetalToolTipUI;

/**
 * Some checks for the getAcceleratorString() method in the 
 * MetalToolTipUI class.
 */
public class getAcceleratorString implements Testlet 
{
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    JButton button1 = new JButton("Test 1");
    button1.setMnemonic(KeyEvent.VK_B);
    JToolTip t1 = button1.createToolTip();
    MetalToolTipUI ui1 = (MetalToolTipUI) t1.getUI();
    harness.check(ui1.getAcceleratorString(), "Alt-B");

    JButton button2 = new JButton("Test 2");
    button2.setMnemonic(KeyEvent.VK_C);
    JToolTip t2 = button2.createToolTip();
    MetalToolTipUI ui2 = (MetalToolTipUI) t2.getUI();
    harness.check(ui2.getAcceleratorString(), "Alt-C");
    harness.check(ui1.getAcceleratorString(), "Alt-C");
    harness.check(ui1 == ui2);
  }

}
