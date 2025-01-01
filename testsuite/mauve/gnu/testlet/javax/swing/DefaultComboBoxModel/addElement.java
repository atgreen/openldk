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
 * Some checks for the addElement() method in the 
 * {@link DefaultComboBoxModel} class.
 */
public class addElement 
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
    DefaultComboBoxModel m = new DefaultComboBoxModel();
    m.addListDataListener(this);
    
    m.addElement("A");
    harness.check(m.getSize(), 1);
    harness.check(m.getElementAt(0), "A");
    harness.check(m.getSelectedItem(), "A");
    harness.check(events.size(), 2);
    ListDataEvent e = (ListDataEvent) events.get(0);
    harness.check(e.getType(), ListDataEvent.INTERVAL_ADDED);
    harness.check(e.getIndex0(), 0);
    harness.check(e.getIndex1(), 0);
    e = (ListDataEvent) events.get(1);
    harness.check(e.getType(), ListDataEvent.CONTENTS_CHANGED);
    harness.check(e.getIndex0(), -1);
    harness.check(e.getIndex1(), -1);
    events.clear();
    
    m.addElement("B");
    harness.check(m.getSize(), 2);
    harness.check(m.getElementAt(1), "B");
    harness.check(m.getSelectedItem(), "A");
    harness.check(events.size(), 1);
    e = (ListDataEvent) events.get(0);
    harness.check(e.getType(), ListDataEvent.INTERVAL_ADDED);
    harness.check(e.getIndex0(), 1);
    harness.check(e.getIndex1(), 1);
    events.clear();
    
    m.addElement(null);
    harness.check(m.getSize(), 3);
    harness.check(m.getElementAt(2), null);
    harness.check(m.getSelectedItem(), "A");
    harness.check(events.size(), 1);
    e = (ListDataEvent) events.get(0);
    harness.check(e.getType(), ListDataEvent.INTERVAL_ADDED);
    harness.check(e.getIndex0(), 2);
    harness.check(e.getIndex1(), 2);
    events.clear();
    
  }
}

