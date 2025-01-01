// Tags: JDK1.4

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

package gnu.testlet.javax.swing.plaf.metal.MetalBorders.ToolBarBorder;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Insets;

import javax.swing.JToolBar;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.plaf.metal.DefaultMetalTheme;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.swing.plaf.metal.MetalBorders.ToolBarBorder;

/**
* Some tests for the getBorderInsets() method in the {@link ToolBarBorder} 
* class.
*/
public class getBorderInsets implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    test1(harness);
    test2(harness);
  }
  
  public void test1(TestHarness harness)      
  {
    harness.checkPoint("getBorderInsets(Component)");
    // test with DefaultMetalTheme
    try
      {
        MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
        UIManager.setLookAndFeel(new MetalLookAndFeel());
      }
    catch (UnsupportedLookAndFeelException e)
      {
        e.printStackTrace();  
      }
    ToolBarBorder b = new ToolBarBorder();
    JToolBar tb = new JToolBar(JToolBar.HORIZONTAL);
    Insets insets1 = b.getBorderInsets(tb);
    harness.check(insets1, new Insets(2, 16, 2, 2));
    tb.setOrientation(JToolBar.VERTICAL);
    Insets insets2 = b.getBorderInsets(tb);
    harness.check(insets2, new Insets(16, 2, 2, 2));
  }

  public void test2(TestHarness harness)      
  {
    harness.checkPoint("getBorderInsets(Component, Insets)");
    // test with DefaultMetalTheme
    try
      {
        MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
        UIManager.setLookAndFeel(new MetalLookAndFeel());
      }
    catch (UnsupportedLookAndFeelException e)
      {
        e.printStackTrace();  
      }
    JToolBar tb = new JToolBar(JToolBar.HORIZONTAL);
    ToolBarBorder b = new ToolBarBorder();
    Insets insets = b.getBorderInsets(tb, new Insets(1, 2, 3, 4));
    harness.check(insets, new Insets(2, 16, 2, 2));
    tb.setOrientation(JToolBar.VERTICAL);
    insets = b.getBorderInsets(tb, new Insets(1, 2, 3, 4));
    harness.check(insets, new Insets(16, 2, 2, 2));
    
    boolean pass = false;
    try
    {
      b.getBorderInsets(null, null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
  
}
