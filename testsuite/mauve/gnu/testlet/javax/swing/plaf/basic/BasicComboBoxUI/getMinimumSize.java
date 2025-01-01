// Tags: JDK1.2
// Uses: MyBasicComboBoxUI

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
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.plaf.basic.BasicComboBoxUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Dimension;
import java.awt.FontMetrics;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.plaf.basic.BasicComboBoxUI;

/**
 * Some checks for the getMinimumSize() method in the 
 * {@link BasicComboBoxUI} class.  
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
    JComboBox cb = new JComboBox();
    MyBasicComboBoxUI ui = new MyBasicComboBoxUI();
    cb.setUI(ui);
    int additionalHeight = 2;  // margin?  border?
    int additionalWidth = 2;  
    FontMetrics fm = cb.getFontMetrics(cb.getFont());
    
    // the following width calculation is a guess.  We know the value
    // depends on the font size, and that it is relatively small, so after
    // trying out a few candidates this one seems to give the right result
    int width = fm.charWidth(' ') + additionalWidth;
    int height = fm.getHeight() + additionalHeight;
    harness.check(ui.getMinimumSize(cb), new Dimension(width + height, height));
                   // the width is the display width plus the button width and
                   // the button width is equal to 'height'
    
    cb.setModel(new DefaultComboBoxModel(new Object[] {"X"}));
    width = fm.charWidth('X') + additionalWidth;
    harness.check(ui.getMinimumSize(cb), new Dimension(width + height, height));
    
    cb.setPrototypeDisplayValue("XX");    
    width = fm.stringWidth("XX") + additionalWidth;
    harness.check(ui.getMinimumSize(cb), new Dimension(width + height, height));
    
  }

}
