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

package gnu.testlet.javax.swing.plaf.metal.MetalBorders.ButtonBorder;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Insets;

import javax.swing.JButton;
import javax.swing.plaf.metal.MetalBorders.ButtonBorder;

/**
* Some tests for the getBorderInsets() method in the {@link ButtonBorder} 
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
    ButtonBorder b = new ButtonBorder();
    Insets insets = b.getBorderInsets(null);
    harness.check(insets, new Insets(3, 3, 3, 3));
    
    // the method always returns the same instance
    Insets insets2 = b.getBorderInsets(null);
    harness.check(insets == insets2);
  }

  public void test2(TestHarness harness)      
  {
    harness.checkPoint("getBorderInsets(Component, Insets)");
    ButtonBorder b = new ButtonBorder();
    Insets insets = b.getBorderInsets(null, new Insets(1, 2, 3, 4));
    harness.check(insets, new Insets(3, 3, 3, 3));

    // check null insets
    boolean pass = false;
    try
    {
      b.getBorderInsets(new JButton("Test"), null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  
  }
  
}
