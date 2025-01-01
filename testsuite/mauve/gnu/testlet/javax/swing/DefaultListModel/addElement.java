// Tags: JDK1.2
// Uses: MyListDataListener

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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.DefaultListModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.DefaultListModel;
import javax.swing.event.ListDataEvent;

/**
 * Some tests for the addElement() method in the {@link DefaultListModel} class.
 */
public class addElement implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("addElement(Object)");
    DefaultListModel m1 = new DefaultListModel();  
    MyListDataListener listener = new MyListDataListener();
    m1.addListDataListener(listener);
    
    m1.addElement("A");
    harness.check(m1.getElementAt(0).equals("A"));
    ListDataEvent event = listener.getEvent();
    harness.check(event.getType(), ListDataEvent.INTERVAL_ADDED);
    harness.check(event.getIndex0(), 0);
    harness.check(event.getIndex1(), 0);
    listener.setListDataEvent(null);
    
    m1.addElement("B");
    harness.check(m1.getElementAt(1).equals("B"));
    event = listener.getEvent();
    harness.check(event.getType(), ListDataEvent.INTERVAL_ADDED);
    harness.check(event.getIndex0(), 1);
    harness.check(event.getIndex1(), 1);
    listener.setListDataEvent(null);

    m1.addElement("C");
    harness.check(m1.getElementAt(2).equals("C"));
    event = listener.getEvent();
    harness.check(event.getType(), ListDataEvent.INTERVAL_ADDED);
    harness.check(event.getIndex0(), 2);
    harness.check(event.getIndex1(), 2);
    listener.setListDataEvent(null);

    m1.addElement(null);
    harness.check(m1.getElementAt(3) == null); 
    event = listener.getEvent();
    harness.check(event.getType(), ListDataEvent.INTERVAL_ADDED);
    harness.check(event.getIndex0(), 3);
    harness.check(event.getIndex1(), 3);
    listener.setListDataEvent(null);
  }

}
