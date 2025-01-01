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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  

package gnu.testlet.javax.swing.DefaultComboBoxModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;

/**
 * Some checks for the setSelectedItem() method in the 
 * {@link DefaultComboBoxModel} class.
 */
public class setSelectedItem 
  implements Testlet, ListDataListener 
{
  List events = new java.util.ArrayList();
  
  public void contentsChanged(ListDataEvent event) 
  {
    events.add(event);
  }
  
  public void intervalAdded(ListDataEvent event) 
  {
    events.add(event);
  }
  
  public void intervalRemoved(ListDataEvent event) 
  {
    events.add(event);
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {   
    DefaultComboBoxModel m = new DefaultComboBoxModel(new Object[] {"A", "B", 
            "C"});
    harness.check(m.getSelectedItem(), "A");
    
    m.addListDataListener(this);

    // first set the selected item to one of the elements in the list...
    m.setSelectedItem("C");
    harness.check(events.size(), 1);
    harness.check(m.getSelectedItem(), "C");
    ListDataEvent event = (ListDataEvent) events.get(0);
    harness.check(event.getType(), ListDataEvent.CONTENTS_CHANGED);
    harness.check(event.getIndex0(), -1);
    harness.check(event.getIndex1(), -1);
    events.clear();
    
    // now set the selected item to null...
    m.setSelectedItem(null);
    harness.check(m.getSelectedItem(), null);
    harness.check(events.size(), 1);
    event = (ListDataEvent) events.get(0);
    harness.check(event.getType(), ListDataEvent.CONTENTS_CHANGED);
    harness.check(event.getIndex0(), -1);
    harness.check(event.getIndex1(), -1);
    events.clear();

    // now set the selected item to something not in the list...
    m.setSelectedItem("Z");
    // confirm that setSelectedItem simply returned without doing
    // anything...
    harness.check(m.getSelectedItem(), null);
    harness.check(m.getSize(), 3);
    harness.check(m.getIndexOf("Z"), -1);
    harness.check(events.size(), 0);

    // now set the selected item to the same value - no event should be 
    // generated...
    m.setSelectedItem("Z");
    harness.check(events.size(), 0);
    
    // make sure setting null when already null doesn't generate an event
    m.setSelectedItem(null);
    events.clear();
    m.setSelectedItem(null);
    harness.check(events.size(), 0);
    
  }
}

