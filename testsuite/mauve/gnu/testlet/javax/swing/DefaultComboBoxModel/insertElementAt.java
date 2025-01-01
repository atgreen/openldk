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

import javax.swing.DefaultComboBoxModel;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;

/**
 * Some checks for the insertElementAt() method in the 
 * {@link DefaultComboBoxModel} class.
 */
public class insertElementAt 
  implements Testlet, ListDataListener 
{
  int index0;
  int index1;
  int eventType;
  
  public void contentsChanged(ListDataEvent event) 
  {
    eventType = event.getType();
    index0 = event.getIndex0();
    index1 = event.getIndex1();
  }
  
  public void intervalAdded(ListDataEvent event) 
  {
    eventType = event.getType();
    index0 = event.getIndex0();
    index1 = event.getIndex1();
  }
  
  public void intervalRemoved(ListDataEvent event) 
  {
    eventType = event.getType();
    index0 = event.getIndex0();
    index1 = event.getIndex1();
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
    
    m.insertElementAt("A", 0);
    harness.check(m.getSize(), 1);
    harness.check(m.getElementAt(0), "A");
    harness.check(m.getSelectedItem(), null);
    harness.check(eventType, ListDataEvent.INTERVAL_ADDED);
    harness.check(index0, 0);
    harness.check(index1, 0);
    
    m.insertElementAt("B", 1);
    harness.check(m.getSize(), 2);
    harness.check(m.getElementAt(1), "B");
    harness.check(m.getSelectedItem(), null);
    harness.check(eventType, ListDataEvent.INTERVAL_ADDED);
    harness.check(index0, 1);
    harness.check(index1, 1);
    
    m.insertElementAt("C", 0);
    harness.check(m.getSize(), 3);
    harness.check(m.getElementAt(0), "C");
    harness.check(m.getSelectedItem(), null);
    harness.check(eventType, ListDataEvent.INTERVAL_ADDED);
    harness.check(index0, 0);
    harness.check(index1, 0);

    m.insertElementAt(null, 0);
    harness.check(m.getSize(), 4);
    harness.check(m.getElementAt(0), null);
    harness.check(m.getSelectedItem(), null);
    harness.check(eventType, ListDataEvent.INTERVAL_ADDED);
    harness.check(index0, 0);
    harness.check(index1, 0);
    
    // try negative index
    boolean pass = true;
    try
    {
      m.insertElementAt("Z", -1);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try index too large
    pass = false;
    try
    {
      m.insertElementAt("Z", 5);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass); 
  }
}

