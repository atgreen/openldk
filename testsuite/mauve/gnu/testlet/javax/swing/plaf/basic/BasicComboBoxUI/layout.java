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

import java.awt.BorderLayout;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Rectangle;

import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.UIManager;
import javax.swing.plaf.basic.BasicComboBoxUI;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some checks for the layout in the {@link BasicComboBoxUI} class.  
 */
public class layout implements Testlet 
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
    JComboBox cb = new JComboBox();
    cb.setEditable(true);
    JFrame frame = new JFrame();
    JPanel panel = new JPanel(new BorderLayout());
    panel.add(cb);
    frame.setContentPane(panel);
    frame.pack();
    JTextField tf = (JTextField) cb.getEditor().getEditorComponent();
    Font font = cb.getFont();
    FontMetrics fm = cb.getFontMetrics(font);
    int height = cb.getHeight();
    int width = fm.stringWidth("m") * tf.getColumns() + 1;
    harness.check(tf.getBounds(), new Rectangle(0, 0, width, height));
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
