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

import java.awt.Rectangle;

import javax.swing.JScrollBar;
import javax.swing.UIManager;
import javax.swing.plaf.basic.BasicScrollBarUI;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some checks for the layoutContainer() method in the 
 * {@link BasicScrollBarUI} class.  
 */
public class layoutContainer implements Testlet 
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
    testHorizontal(harness);
    testVertical(harness);
    
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
  
  public void testHorizontal(TestHarness harness)  
  {
    MyBasicScrollBarUI ui = new MyBasicScrollBarUI();
        JScrollBar scrollBar = new JScrollBar(JScrollBar.HORIZONTAL);
    scrollBar.setUI(ui);
    scrollBar.setBounds(0, 0, 100, 20);
    ui.layoutContainer(scrollBar);
    harness.check(ui.getTrackBounds(), new Rectangle(16, 0, 68, 20));    
    harness.check(ui.getThumbBounds(), new Rectangle(16, 0, 8, 20));
    
    scrollBar.setBounds(40, 50, 250, 37);
    ui.layoutContainer(scrollBar);
    harness.check(ui.getTrackBounds(), new Rectangle(16, 0, 218, 37));    
    harness.check(ui.getThumbBounds(), new Rectangle(16, 0, 21, 37));
  }

  public void testVertical(TestHarness harness)  
  {
    MyBasicScrollBarUI ui = new MyBasicScrollBarUI();
        JScrollBar scrollBar = new JScrollBar(JScrollBar.VERTICAL);
    scrollBar.setUI(ui);
    scrollBar.setBounds(0, 0, 20, 100);
    ui.layoutContainer(scrollBar);
    harness.check(ui.getTrackBounds(), new Rectangle(0, 16, 20, 68));    
    harness.check(ui.getThumbBounds(), new Rectangle(0, 16, 20, 8));
    
    scrollBar.setBounds(40, 50, 37, 250);
    ui.layoutContainer(scrollBar);
    harness.check(ui.getTrackBounds(), new Rectangle(0, 16, 37, 218));    
    harness.check(ui.getThumbBounds(), new Rectangle(0, 16, 37, 21));
  }

}
