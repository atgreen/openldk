/* setColumnSelectionAllowed.java -- some checks for the 
       setColumnSelectionAllowed() method in the JTable class.
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

// Tags: JDK1.4

package gnu.testlet.javax.swing.JTable;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.JTable;

public class setColumnSelectionAllowed 
  implements Testlet, PropertyChangeListener 
{
  List events = new java.util.ArrayList();
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    events.add(e);
  }
 
  public void test(TestHarness harness) 
  {
    JTable t = new JTable();
    harness.check(t.getColumnSelectionAllowed(), false);
    t.addPropertyChangeListener(this);
    t.setColumnSelectionAllowed(true);
    harness.check(events.size(), 1);
    PropertyChangeEvent event = (PropertyChangeEvent) events.get(0);
    harness.check(event.getSource(), t);
    harness.check(event.getPropertyName(), "columnSelectionAllowed");
    harness.check(event.getOldValue(), Boolean.FALSE);
    harness.check(event.getNewValue(), Boolean.TRUE);
    
    // setting the same again generates no event
    events.clear();
    t.setRowSelectionAllowed(true);
    harness.check(events.size(), 0);
  }
}

