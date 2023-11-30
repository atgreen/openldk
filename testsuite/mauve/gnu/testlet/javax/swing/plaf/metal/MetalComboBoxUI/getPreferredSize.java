// Tags: JDK1.5
// Uses: MyMetalComboBoxUI

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

package gnu.testlet.javax.swing.plaf.metal.MetalComboBoxUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.plaf.metal.MetalComboBoxButton;
import javax.swing.plaf.metal.MetalComboBoxUI;

/**
 * Some checks for the getPreferredSize() method in the 
 * {@link MetalComboBoxUI} class.  
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
    testNonEditable(harness);
    testEditable(harness);
  }
  
  private void testNonEditable(TestHarness harness)  
  {
    harness.checkPoint("testNonEditable()");
    JComboBox cb = new JComboBox();
    MyMetalComboBoxUI ui = new MyMetalComboBoxUI();
    cb.setUI(ui);
    Insets insets = ui.getArrowButton().getInsets();
    Insets comboInsets = cb.getInsets();
    MetalComboBoxButton b = (MetalComboBoxButton) ui.getArrowButton();
    Dimension displaySize = ui.getDisplaySize();
    int width = displaySize.width + insets.left + 2 * insets.right
                + comboInsets.left + comboInsets.right
                + b.getComboIcon().getIconWidth();
    int height = displaySize.height + comboInsets.top + comboInsets.bottom
                 + insets.top + insets.bottom;
    harness.check(ui.getPreferredSize(cb), new Dimension(width, height));
                   // the width is the display width plus the button width and
                   // the button width is equal to 'height'
    
    cb.setModel(new DefaultComboBoxModel(new Object[] {"X"}));
    displaySize = ui.getDisplaySize();
    width = displaySize.width + insets.left + 2 * insets.right
                + comboInsets.left + comboInsets.right
                + b.getComboIcon().getIconWidth();
    height = displaySize.height + comboInsets.top + comboInsets.bottom
             + insets.top + insets.bottom;
    harness.check(ui.getPreferredSize(cb), new Dimension(width, height));
    
    cb.setPrototypeDisplayValue("XX");    
    displaySize = ui.getDisplaySize();
    width = displaySize.width + insets.left + 2 * insets.right
                + comboInsets.left + comboInsets.right
                + b.getComboIcon().getIconWidth();
    height = displaySize.height + comboInsets.top + comboInsets.bottom
             + insets.top + insets.bottom;
    harness.check(ui.getPreferredSize(cb), new Dimension(width, height));
    
  }

  private void testEditable(TestHarness harness)
  {
    harness.checkPoint("testEditable()");
    JComboBox cb = new JComboBox();
    MyMetalComboBoxUI ui = new MyMetalComboBoxUI();
    cb.setUI(ui);
    cb.setEditable(true);
    // Check empty ComboBox.
    Dimension dSize = ui.getDisplaySize();
    Insets i = cb.getInsets();
    Insets arrowMargin = ui.getArrowButton().getMargin();
    int width = dSize.width + i.left + i.right
                + dSize.height + arrowMargin.left + arrowMargin.right;
    int height = dSize.height + i.top + i.bottom 
                              + arrowMargin.top + arrowMargin.bottom;
    harness.check(ui.getPreferredSize(cb), new Dimension(width, height));

    // Check ComboBox with one element.
    cb.setModel(new DefaultComboBoxModel(new Object[] {"X"}));
    harness.check(ui.getPreferredSize(cb), new Dimension(width, height));        
    cb.setPrototypeDisplayValue("XX");    
    harness.check(ui.getPreferredSize(cb), new Dimension(width, height));
    
    // repeat the tests with a different font
    JComboBox cb2 = new JComboBox();
    MyMetalComboBoxUI ui2 = new MyMetalComboBoxUI();
    cb2.setUI(ui2);
    cb2.setEditable(true);
    cb2.setFont(new Font("Dialog", Font.PLAIN, 24));
    dSize = ui2.getDisplaySize();
    i = cb2.getInsets();
    arrowMargin = ui2.getArrowButton().getMargin();
    width = dSize.width + i.left + i.right
            + dSize.height + arrowMargin.left + arrowMargin.right;
    height = dSize.height + i.top + i.bottom 
             + arrowMargin.top + arrowMargin.bottom;
    harness.check(ui2.getPreferredSize(cb2), new Dimension(width, height));    
    cb2.setModel(new DefaultComboBoxModel(new Object[] {"X"}));
    harness.check(ui2.getPreferredSize(cb2), new Dimension(width, height));        
    cb2.setPrototypeDisplayValue("XX");    
    harness.check(ui2.getPreferredSize(cb2), new Dimension(width, height));
  }
  
}
