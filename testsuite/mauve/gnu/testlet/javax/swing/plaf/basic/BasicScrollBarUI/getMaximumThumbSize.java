// Tags: JDK1.2
// Uses: MyBasicScrollBarUI ../../TestLookAndFeel

// Copyright (C) 2005, 2006 David Gilbert <david.gilbert@object-refinery.com>

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

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.javax.swing.plaf.TestLookAndFeel;

import java.awt.Dimension;

import javax.swing.JScrollBar;
import javax.swing.UIManager;
import javax.swing.plaf.basic.BasicScrollBarUI;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some checks for the getMaximumThumbSize() method in the 
 * {@link BasicScrollBarUI} class.  
 */
public class getMaximumThumbSize implements Testlet 
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
      UIManager.setLookAndFeel(new TestLookAndFeel());
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }

    // test horizontal scroll bar
    MyBasicScrollBarUI ui = new MyBasicScrollBarUI();
    JScrollBar scrollBar = new JScrollBar(JScrollBar.HORIZONTAL);
    scrollBar.setUI(ui);
    harness.check(ui.getMaximumThumbSize(), new Dimension(4096, 4096));
  
    // test vertical scroll bar
    MyBasicScrollBarUI ui2 = new MyBasicScrollBarUI();
    JScrollBar scrollBar2 = new JScrollBar(JScrollBar.VERTICAL);
    scrollBar2.setUI(ui2);
    harness.check(ui2.getMaximumThumbSize(), new Dimension(4096, 4096));
    
    // restore a sane look and feel
    try
    {
      UIManager.setLookAndFeel(new MetalLookAndFeel());
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }
  }

}
