// Tags: JDK1.2
// Uses: MyMetalScrollBarUI

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

package gnu.testlet.javax.swing.plaf.metal.MetalScrollBarUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Dimension;

import javax.swing.JScrollBar;
import javax.swing.UIManager;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.swing.plaf.metal.MetalScrollBarUI;

/**
 * Some checks for the getPreferredSize() method in the {@link MetalScrollBarUI} 
 * class.  
 */
public class getPreferredSize implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    // use a known look and feel
    try
    {
      UIManager.setLookAndFeel(new MetalLookAndFeel());
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }

    // test horizontal scroll bar
    MyMetalScrollBarUI ui = new MyMetalScrollBarUI();
    JScrollBar scrollBar = new JScrollBar(JScrollBar.HORIZONTAL);
    scrollBar.setUI(ui);
    harness.check(ui.getPreferredSize(scrollBar), new Dimension(61, 17));
  
    // test vertical scroll bar
    MyMetalScrollBarUI ui2 = new MyMetalScrollBarUI();
    JScrollBar scrollBar2 = new JScrollBar(JScrollBar.VERTICAL);
    scrollBar2.setUI(ui2);
    harness.check(ui2.getPreferredSize(scrollBar2), new Dimension(17, 61));
  }

}
