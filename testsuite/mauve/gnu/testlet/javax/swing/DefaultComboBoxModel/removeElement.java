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

import java.util.ArrayList;
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;

/**
 * Some checks for the removeElement() method in the 
 * {@link DefaultComboBoxModel} class.
 */
public class removeElement 
  implements Testlet, ListDataListener 
{
  List events = new ArrayList();
  
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
    DefaultComboBoxModel m = new DefaultComboBoxModel(new Object[] {"A", "B", "C"});
    m.addListDataListener(this);
    
    m.removeElement("A");
    harness.check(m.getSize(), 2);
    harness.check(m.getElementAt(0), "B");
    harness.check(m.getSelectedItem(), "B");
    harness.check(events.size(), 2);
    ListDataEvent e0 = (ListDataEvent) events.get(0);
    harness.check(e0.getType(), ListDataEvent.CONTENTS_CHANGED);
    harness.check(e0.getIndex0(), -1);
    harness.check(e0.getIndex1(), -1);
    ListDataEvent e1 = (ListDataEvent) events.get(1);
    harness.check(e1.getType(), ListDataEvent.INTERVAL_REMOVED);
    harness.check(e1.getIndex0(), 0);
    harness.check(e1.getIndex1(), 0);

    events.clear();
    m.removeElement("C");
    harness.check(m.getSize(), 1);
    harness.check(m.getElementAt(0), "B");
    harness.check(m.getSelectedItem(), "B");
    harness.check(events.size(), 1);
    e0 = (ListDataEvent) events.get(0);
    harness.check(e0.getType(), ListDataEvent.INTERVAL_REMOVED);
    harness.check(e0.getIndex0(), 1);
    harness.check(e0.getIndex1(), 1);

    
    events.clear();

    m.removeElement("Z");
    harness.check(m.getSize(), 1);
    harness.check(m.getSelectedItem(), "B");
    harness.check(events.size(), 0);
    
  }
}

