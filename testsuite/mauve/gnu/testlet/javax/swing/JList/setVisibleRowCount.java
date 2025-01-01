/* setVisibleRowCount.java -- some checks for the setVisibleRowCount() method
       in the JList class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.2

package gnu.testlet.javax.swing.JList;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.JList;

public class setVisibleRowCount implements Testlet, PropertyChangeListener
{
  List events = new java.util.ArrayList();
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    events.add(e);
  }

  public void test(TestHarness harness)
  {
    JList list = new JList();
    harness.check(list.getVisibleRowCount(), 8);
    list.addPropertyChangeListener(this);
    list.setVisibleRowCount(13);
    harness.check(list.getVisibleRowCount(), 13);
    harness.check(events.size(), 1);
    PropertyChangeEvent e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getSource(), list);
    harness.check(e0.getPropertyName(), "visibleRowCount");
    harness.check(e0.getOldValue(), new Integer(8));
    harness.check(e0.getNewValue(), new Integer(13));
    
    // setting the same value generates no event
    events.clear();
    list.setVisibleRowCount(13);
    harness.check(events.size(), 0);
    
    // try zero
    list.setVisibleRowCount(0);
    harness.check(list.getVisibleRowCount(), 0);
    
    // try a negative value
    events.clear();
    list.setVisibleRowCount(-1);
    harness.check(list.getVisibleRowCount(), 0);
    harness.check(events.size(), 1);
    e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getSource(), list);
    harness.check(e0.getPropertyName(), "visibleRowCount");
    harness.check(e0.getOldValue(), new Integer(0));
    harness.check(e0.getNewValue(), new Integer(-1));
        
    events.clear();
    list.setVisibleRowCount(-99);
    harness.check(list.getVisibleRowCount(), 0);
    e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getSource(), list);
    harness.check(e0.getPropertyName(), "visibleRowCount");
    harness.check(e0.getOldValue(), new Integer(0));
    harness.check(e0.getNewValue(), new Integer(-99));
  }
}
