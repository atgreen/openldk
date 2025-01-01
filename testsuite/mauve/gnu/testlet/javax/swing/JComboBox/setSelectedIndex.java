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

import javax.swing.JComboBox;

/**
 * Some checks for the setSelectedIndex() method in the 
 * {@link JComboBox} class.
 */
public class setSelectedIndex
  implements Testlet, ItemListener 
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
    
    c.addItemListener(this);
    
    c.setSelectedIndex(1);
    harness.check(c.getSelectedItem(), "B");
    harness.check(events.size(), 2);
    ItemEvent e1 = (ItemEvent) events.get(0);
    harness.check(e1.getStateChange(), ItemEvent.DESELECTED);
    harness.check(e1.getItem(), "A");
    ItemEvent e2 = (ItemEvent) events.get(1);
    harness.check(e2.getStateChange(), ItemEvent.SELECTED);
    harness.check(e2.getItem(), "B");
    events.clear();
    
    c.setSelectedIndex(-1);
    harness.check(events.size(), 1);
    e1 = (ItemEvent) events.get(0);
    harness.check(e1.getStateChange(), ItemEvent.DESELECTED);
    harness.check(e1.getItem(), "B");
    events.clear();
    
    // no change == no event
    c.setSelectedIndex(-1);
    harness.check(events.size(), 0);
    
    c.setSelectedIndex(1);
    events.clear();
    c.setSelectedIndex(1);
    harness.check(events.size(), 0);
  }
  
  private List events = new ArrayList();
  
  public void itemStateChanged(ItemEvent e)
  {
    events.add(e);
  }
  

}

