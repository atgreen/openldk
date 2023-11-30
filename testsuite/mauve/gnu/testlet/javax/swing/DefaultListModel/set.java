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
 * Some tests for the set() method in the {@link DefaultListModel} class.
 */
public class set implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("set(int, Object)");
    DefaultListModel m1 = new DefaultListModel();
    MyListDataListener listener = new MyListDataListener();
    m1.addListDataListener(listener);
    m1.addElement("A");
    m1.addElement("B");
    m1.addElement("C");
    
    Object old = m1.set(0, "X");
    harness.check(m1.get(0), "X");
    harness.check(old, "A");
    ListDataEvent event = listener.getEvent();
    harness.check(event.getType(), ListDataEvent.CONTENTS_CHANGED);
    harness.check(event.getIndex0(), 0);
    harness.check(event.getIndex1(), 0);
    listener.setListDataEvent(null);
        
    old = m1.set(1, "Y");
    harness.check(m1.get(1), "Y");
    harness.check(old, "B");
    event = listener.getEvent();
    harness.check(event.getType(), ListDataEvent.CONTENTS_CHANGED);
    harness.check(event.getIndex0(), 1);
    harness.check(event.getIndex1(), 1);
    listener.setListDataEvent(null);
    
    old = m1.set(2, "Z");
    harness.check(m1.get(2), "Z");
    harness.check(old, "C");
    event = listener.getEvent();
    harness.check(event.getType(), ListDataEvent.CONTENTS_CHANGED);
    harness.check(event.getIndex0(), 2);
    harness.check(event.getIndex1(), 2);
    listener.setListDataEvent(null);
    
    old = m1.set(1, null);
    harness.check(m1.get(1), null);
    harness.check(old, "Y");
    event = listener.getEvent();
    harness.check(event.getType(), ListDataEvent.CONTENTS_CHANGED);
    harness.check(event.getIndex0(), 1);
    harness.check(event.getIndex1(), 1);
    listener.setListDataEvent(null);
    
    boolean pass = false;
    try
    {
      m1.set(-1, "ZZ");
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);

    pass = false;
    try
    {
      m1.set(3, "ZZ");
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);
  
  }

}
