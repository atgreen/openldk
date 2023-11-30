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
import java.awt.FontMetrics;
import java.awt.Insets;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.plaf.metal.DefaultMetalTheme;
import javax.swing.plaf.metal.MetalComboBoxButton;
import javax.swing.plaf.metal.MetalComboBoxIcon;
import javax.swing.plaf.metal.MetalComboBoxUI;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some checks for the getMinimumSize() method in the 
 * {@link MetalComboBoxUI} class.  
 */
public class getMinimumSize implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    // test with a known LAF/theme
    try
    {
      MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
      UIManager.setLookAndFeel(new MetalLookAndFeel());
    }
    catch (UnsupportedLookAndFeelException e)
    {
      e.printStackTrace();
    }
    test1(harness);
    test2(harness);
    testEditable(harness);
    testEditableWithCustomFont(harness);
  }

  public void test1(TestHarness harness)  
  {
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
    harness.check(ui.getMinimumSize(cb), new Dimension(width, height));
                   // the width is the display width plus the button width and
                   // the button width is equal to 'height'
    
    cb.setModel(new DefaultComboBoxModel(new Object[] {"X"}));
    displaySize = ui.getDisplaySize();
    width = displaySize.width + insets.left + 2 * insets.right
                + comboInsets.left + comboInsets.right
                + b.getComboIcon().getIconWidth();
    height = displaySize.height + comboInsets.top + comboInsets.bottom
             + insets.top + insets.bottom;
    harness.check(ui.getMinimumSize(cb), new Dimension(width, height));
    
    cb.setPrototypeDisplayValue("XX");    
    displaySize = ui.getDisplaySize();
    width = displaySize.width + insets.left + 2 * insets.right
                + comboInsets.left + comboInsets.right
                + b.getComboIcon().getIconWidth();
    height = displaySize.height + comboInsets.top + comboInsets.bottom
             + insets.top + insets.bottom;
    harness.check(ui.getMinimumSize(cb), new Dimension(width, height));
  }

  /**
   * Checks the minimum size for a non-editable JComboBox with a custom font.
   * 
   * @param harness
   */
  public void test2(TestHarness harness)  
  {
    JComboBox cb = new JComboBox();
    cb.setFont(new Font("Dialog", Font.PLAIN, 24));
    MyMetalComboBoxUI ui = new MyMetalComboBoxUI();
    cb.setUI(ui);
    MetalComboBoxButton button = (MetalComboBoxButton) ui.getArrowButton();
    MetalComboBoxIcon icon = (MetalComboBoxIcon) button.getComboIcon();
    Insets insets = button.getInsets();
    int additionalWidth = insets.left + insets.right + 2;
    int additionalHeight = insets.top + insets.bottom + 2;
    int iconWidth = icon.getIconWidth() + 6;  // a margin = 6 by trial and 
                                              // error
    FontMetrics fm = cb.getFontMetrics(cb.getFont());

    // the following width calculation is a guess.  We know the value
    // depends on the font size, and that it is relatively small, so after
    // trying out a few candidates this one seems to give the right result
    int width = fm.charWidth(' ') + additionalWidth + iconWidth;
    int height = fm.getHeight() + additionalHeight;
    harness.check(ui.getMinimumSize(cb), new Dimension(width, height));
                   // the width is the display width plus the button width and
                   // the button width is equal to 'height'
    
    cb.setModel(new DefaultComboBoxModel(new Object[] {"X"}));
    width = fm.charWidth('X') + additionalWidth + iconWidth;
    harness.check(ui.getMinimumSize(cb), new Dimension(width, height));
    
    cb.setPrototypeDisplayValue("XX");    
    width = fm.stringWidth("XX") + additionalWidth + iconWidth;
    harness.check(ui.getMinimumSize(cb), new Dimension(width, height));

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
    harness.check(ui.getMinimumSize(cb), new Dimension(width, height));

    // Check ComboBox with one element.
    cb.setModel(new DefaultComboBoxModel(new Object[] {"X"}));
    harness.check(ui.getMinimumSize(cb), new Dimension(width, height));        
    cb.setPrototypeDisplayValue("XX");    
    harness.check(ui.getMinimumSize(cb), new Dimension(width, height));
    
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
    harness.check(ui2.getMinimumSize(cb2), new Dimension(width, height));    
    cb2.setModel(new DefaultComboBoxModel(new Object[] {"X"}));
    harness.check(ui2.getMinimumSize(cb2), new Dimension(width, height));        
    cb2.setPrototypeDisplayValue("XX");    
    harness.check(ui2.getMinimumSize(cb2), new Dimension(width, height));
  }

  /**
   * Checks the minimum size for an editable JComboBox with a custom font.
   * 
   * @param harness
   */
  public void testEditableWithCustomFont(TestHarness harness)  
  {
    harness.checkPoint("testEditableWithCustomFont()");
    JComboBox cb = new JComboBox();
    cb.setFont(new Font("Dialog", Font.PLAIN, 24));
    MyMetalComboBoxUI ui = new MyMetalComboBoxUI();
    cb.setUI(ui);
    cb.setEditable(true);
    Dimension dSize = ui.getDisplaySize();
    Insets i = cb.getInsets();
    Insets arrowMargin = ui.getArrowButton().getMargin();
    int width = dSize.width + i.left + i.right
                + dSize.height + arrowMargin.left + arrowMargin.right;
    int height = dSize.height + i.top + i.bottom 
                              + arrowMargin.top + arrowMargin.bottom;
    harness.check(ui.getPreferredSize(cb), new Dimension(width, height));    
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
