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

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;

import javax.swing.JComboBox;
import javax.swing.ListCellRenderer;
import javax.swing.UIManager;
import javax.swing.plaf.basic.BasicComboBoxUI;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some checks for the createRenderer() method in the 
 * {@link BasicComboBoxUI} class.  
 */
public class createRenderer implements Testlet 
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
    
    // use this to check that the defaults are just those for a JLabel
    UIManager.put("Label.font", new Font("Dialog", Font.PLAIN, 21));
    UIManager.put("Label.foreground", Color.green);
    UIManager.put("Label.background", Color.yellow);
    
    MyBasicComboBoxUI ui = new MyBasicComboBoxUI();
    ListCellRenderer renderer = ui.createRenderer();
    Component c = (Component) renderer;
    
    harness.check(c.getFont(), new Font("Dialog", Font.PLAIN, 21));
    harness.check(c.getForeground(), Color.green);
    harness.check(c.getBackground(), Color.yellow);
    
    // confirm that the method creates a new instance every time
    ListCellRenderer renderer2 = ui.createRenderer();
    harness.check(renderer != renderer2);
    
    JComboBox cb = new JComboBox();
    ListCellRenderer renderer3 = cb.getRenderer();
    Component c3 = (Component) renderer3;
    harness.check(c3.getFont(), new Font("Dialog", Font.PLAIN, 21));
    harness.check(c3.getForeground(), Color.green);
    harness.check(c3.getBackground(), Color.yellow);
    
    // changing the font on the combo box doesn't update the renderer font
    cb.setFont(new Font("Dialog", Font.BOLD, 10));
    harness.check(c3.getFont(), new Font("Dialog", Font.PLAIN, 21));
    ListCellRenderer renderer4 = cb.getRenderer();
    harness.check(renderer3 == renderer4);
    
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
