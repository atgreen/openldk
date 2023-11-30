// Tags: JDK1.4
// Uses: MyJComboBox

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

import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;

/**
 * Some checks for the setModel() method in the 
 * {@link JComboBox} class.
 */
public class setModel
  implements Testlet, ItemListener, ListDataListener
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {   
    MyJComboBox c = new MyJComboBox(new Object[] {"A", "B", "C"});
    harness.check(c.getSelectedItemReminder(), "A");
    
    c.setModel(new DefaultComboBoxModel(new Object[] {"W", "X", "Y", "Z"}));
    harness.check(c.getSelectedItemReminder(), "W");    
  }
  
  private List events = new ArrayList();
  
  public void itemStateChanged(ItemEvent e)
  {
    events.add(e);
  }
  
  public void intervalAdded(ListDataEvent e) 
  {
    events.add(e);    
  }
  
  public void intervalRemoved(ListDataEvent e)
  {
    events.add(e);
  }
  
  public void contentsChanged(ListDataEvent e)
  {
    events.add(e);
  }

}

