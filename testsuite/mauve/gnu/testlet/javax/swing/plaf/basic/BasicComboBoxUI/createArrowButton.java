// Tags: JDK1.2
// Uses: MyBasicComboBoxUILAF

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

package gnu.testlet.javax.swing.plaf.basic.BasicComboBoxUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Insets;

import javax.swing.JButton;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.plaf.BorderUIResource.CompoundBorderUIResource;
import javax.swing.plaf.basic.BasicArrowButton;
import javax.swing.plaf.basic.BasicComboBoxUI;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some checks for the createArrowButton() method in the 
 * {@link BasicComboBoxUI} class.  
 */
public class createArrowButton implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    try
    {
      UIManager.setLookAndFeel(new MyBasicComboBoxUILAF());
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }
    MyBasicComboBoxUI ui = new MyBasicComboBoxUI();
    JButton b = ui.createArrowButton();
    harness.check(b instanceof BasicArrowButton);
    Border border = b.getBorder();
    harness.check(border instanceof CompoundBorderUIResource);
    // the insets and margin are presumably ignored when this button is
    // drawn...
    Insets insets = b.getInsets();
    harness.check(insets, new Insets(4, 17, 5, 17));
    Insets margin = b.getMargin();
    harness.check(margin, new Insets(2, 14, 2, 14));
    
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
