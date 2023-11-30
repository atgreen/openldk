// Tags: JDK1.4

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

import javax.swing.JComboBox;
import javax.swing.UIManager;

/**
 * Some checks for the removeItem() method in the {@link JComboBox} class.
 */
public class removeItem 
  implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {   
    // use a known look and feel
    try
      {
        UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
      }
    catch (Exception e)
      {
        harness.fail("Problem setting MetalLookAndFeel");
      }
    
    JComboBox c1 = new JComboBox();
    harness.check(c1.getSelectedIndex(), -1);
    harness.check(c1.getSelectedItem(), null);
    
    // remove an item when it doesn't exist
    c1.removeItem("Item 1");
    harness.check(c1.getSelectedIndex(), -1);
    harness.check(c1.getSelectedItem(), null);
    
    c1.addItem("Item 1");
    harness.check(c1.getSelectedItem(), "Item 1");
    c1.addItem("Item 2");
    harness.check(c1.getSelectedItem(), "Item 1");
    c1.addItem("Item 3");
    harness.check(c1.getSelectedItem(), "Item 1");
    c1.setSelectedItem("Item 3");
    harness.check(c1.getSelectedItem(), "Item 3");
    c1.removeItem("Item 3");
    harness.check(c1.getSelectedItem(), "Item 2");

    // check null
    c1.removeItem(null);
    harness.check(c1.getSelectedItem(), "Item 2");

    c1.removeItem("Item 2");
    harness.check(c1.getSelectedItem(), "Item 1");
    harness.check(c1.getSelectedIndex(), 0);
    
    c1.addItem("Item A");
    c1.removeItem("Item 1");
    harness.check(c1.getSelectedItem(), "Item A");
    harness.check(c1.getSelectedIndex(), 0);
  }
  
}

