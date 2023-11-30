// Tags: JDK1.2

// Copyright (C) 2005, 2006, David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.javax.swing.table.TableColumn;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.table.TableColumn;

/**
 * Some tests for the setWidth() method in the {@link TableColumn} class.
 */
public class setWidth implements Testlet, PropertyChangeListener 
{

  List events = new java.util.ArrayList();
  
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    TableColumn c = new TableColumn();
    harness.check(c.getWidth(), 75);
    c.addPropertyChangeListener(this);
    
    c.setWidth(19);
    harness.check(c.getWidth(), 19);
    harness.check(events.size(), 1);
    PropertyChangeEvent e = (PropertyChangeEvent) events.get(0);
    harness.check(e.getPropertyName(), "width");
    harness.check(e.getOldValue(), new Integer(75));
    harness.check(e.getNewValue(), new Integer(19));
    
    c.setWidth(10);
    harness.check(c.getWidth(), 15);
    
    c.setMaxWidth(100);
    c.setWidth(110);
    harness.check(c.getWidth(), 100);
  }
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    this.events.add(e);
  }

}
