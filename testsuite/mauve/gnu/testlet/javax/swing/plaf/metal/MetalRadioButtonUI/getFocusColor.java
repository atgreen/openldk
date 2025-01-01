// Tags: JDK1.2 
// Uses: MyMetalRadioButtonUI

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
// along with GNU Classpath; see the file COPYING.  If not, write to the
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301 USA.


package gnu.testlet.javax.swing.plaf.metal.MetalRadioButtonUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JRadioButton;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.metal.DefaultMetalTheme;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some checks for the getFocusColor() method.
 */
public class getFocusColor implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    // test with a known theme
    try
      {
        MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
        UIManager.setLookAndFeel(new MetalLookAndFeel());
      }
    catch (UnsupportedLookAndFeelException e)
      {
        e.printStackTrace();   
      }
    
    // a new instance has no select color yet
    MyMetalRadioButtonUI ui = new MyMetalRadioButtonUI();
    harness.check(ui.getFocusColor(), null);
    
    // check the setting after a call to installDefaults
    JRadioButton b = new JRadioButton("Test");
    ui.installDefaults(b);
    harness.check(ui.getFocusColor(), new ColorUIResource(153, 153, 204));
  }
  
}
