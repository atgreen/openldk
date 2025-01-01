// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.javax.swing.JComboBox;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

/**
 * Some checks for the communication between the model and the JComboBox.
 */
public class model
  implements Testlet
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
      UIManager.setLookAndFeel(new javax.swing.plaf.metal.MetalLookAndFeel());
    }
    catch (UnsupportedLookAndFeelException e) {
      harness.debug(e);
    }
    DefaultComboBoxModel model = new DefaultComboBoxModel(new Object[] {"A", 
            "B", "C"});
    JComboBox cb = new JComboBox(model);
    harness.check(cb.getModel(), model);
    harness.check(cb.getSelectedIndex(), 0);
    
    // setting the model updates the combo box
    model.setSelectedItem("B");
    harness.check(cb.getSelectedItem(), "B");
    
    // setting the combo box updates the model
    cb.setSelectedItem("C");
    harness.check(model.getSelectedItem(), "C");
  }
  

}

