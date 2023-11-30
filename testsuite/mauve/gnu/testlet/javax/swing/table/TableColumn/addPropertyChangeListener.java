/* addPropertyChangeListener.java -- some checks for the 
       addPropertyChangeListener() method in the TableColumn class.
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

package gnu.testlet.javax.swing.table.TableColumn;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.table.TableColumn;

public class addPropertyChangeListener implements Testlet 
{
  static class Listener implements PropertyChangeListener
  {
    public List events = new java.util.ArrayList();
    
    public void propertyChange(PropertyChangeEvent e)
    {
      events.add(e);
    }
  }
  
  public void test(TestHarness harness)
  {
    TableColumn c = new TableColumn();
    
    // add a listener, make sure it receives an event
    Listener l1 = new Listener();
    c.addPropertyChangeListener(l1);
    c.setWidth(60);
    harness.check(l1.events.size(), 1);
    c.removePropertyChangeListener(l1);
    l1.events.clear();
    c.setWidth(61);
    harness.check(l1.events.size(), 0);
    PropertyChangeListener[] listeners = c.getPropertyChangeListeners();
    harness.check(listeners.length, 0);
    
    // add a null listener
    c.addPropertyChangeListener(null);
    listeners = c.getPropertyChangeListeners();
    harness.check(listeners.length, 0);    
  }
}
