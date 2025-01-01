// Tags: JDK1.2
// Uses: MyMetalComboBoxUI ../../basic/BasicComboBoxUI/MyBasicComboBoxUI

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
import gnu.testlet.javax.swing.plaf.TestLookAndFeel;
import gnu.testlet.javax.swing.plaf.basic.BasicComboBoxUI.MyBasicComboBoxUI;

import java.awt.Dimension;
import java.awt.FontMetrics;

import javax.swing.JComboBox;
import javax.swing.JTextField;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.plaf.metal.DefaultMetalTheme;
import javax.swing.plaf.metal.MetalComboBoxUI;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some checks for the getDisplaySize() method in the 
 * {@link MetalComboBoxUI} class.  This is an inherited method.  
 */
public class getDisplaySize implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    testNotEditable(harness);
    testEditable(harness);
  }
  
  /**
   * Run some checks for a JComboBox that is not editable.
   * 
   * @param harness  the test harness.
   */
  private void testNotEditable(TestHarness harness)
  {
    try
    {
      MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
      UIManager.setLookAndFeel(new MetalLookAndFeel());
    }
    catch (UnsupportedLookAndFeelException e)
    {
      e.printStackTrace();
    }
    JComboBox cb = new JComboBox();
    MyMetalComboBoxUI ui = new MyMetalComboBoxUI();
    cb.setUI(ui);
    int additionalHeight = 2;  // margin?  border?
    int additionalWidth = 2;  
    FontMetrics fm = cb.getFontMetrics(cb.getFont());
    
    // the following width calculation is a guess.  We know the value
    // depends on the font size, and that it is relatively small, so after
    // trying out a few candidates this one seems to give the right result
    int width = fm.charWidth(' ') + additionalWidth;
    int height = fm.getHeight() + additionalHeight;
    harness.check(ui.getDisplaySize(), new Dimension(width, height));
    
    cb.addItem("ABC");
    width = fm.stringWidth("ABC") + additionalWidth;
    harness.check(ui.getDisplaySize(), new Dimension(width, height));

    cb.addItem("A longer item");
    width = fm.stringWidth("A longer item") + additionalWidth;
    harness.check(ui.getDisplaySize(), new Dimension(width, height));

    cb.setPrototypeDisplayValue("Prototype");
    width = fm.stringWidth("Prototype") + additionalWidth;
    harness.check(ui.getDisplaySize(), new Dimension(width, height));
  }

  /**
   * Run some checks for a JComboBox that is editable.
   * 
   * @param harness  the test harness.
   */
  private void testEditable(TestHarness harness)
  {
    try
    {
      UIManager.setLookAndFeel(new TestLookAndFeel());
    }
    catch (UnsupportedLookAndFeelException e)
    {
      e.printStackTrace();
    }
    JComboBox cb = new JComboBox();
    cb.setEditable(true);
    MyBasicComboBoxUI ui = new MyBasicComboBoxUI();
    cb.setUI(ui);
    JTextField tf = (JTextField) cb.getEditor().getEditorComponent();
    int columns = tf.getColumns();
    FontMetrics fm = cb.getFontMetrics(cb.getFont());
    
    // for an editable JComboBox the display size is calculated from the 
    // preferred size of the JTextField it seems, or the prototype display
    // value in some cases...
    int width = fm.charWidth('m') * columns;
    int height = fm.getHeight() + 2;  // the 2 seems to be a fixed margin
    
    // not sure why the width here needs + 1..
    harness.check(ui.getDisplaySize(), new Dimension(width + 1, height));
    
    cb.addItem("ABC");
    harness.check(ui.getDisplaySize(), new Dimension(width, height));

    cb.addItem("A longer item");
    harness.check(ui.getDisplaySize(), new Dimension(width, height));

    cb.setPrototypeDisplayValue("Prototype");
    harness.check(ui.getDisplaySize(), new Dimension(width, height));

    cb.setPrototypeDisplayValue("Long Prototype Display Value");
    width = fm.stringWidth("Long Prototype Display Value") + 2;
    harness.check(ui.getDisplaySize(), new Dimension(width, height));
    
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
