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
 * Some tests for the setMinWidth() method in the {@link TableColumn} class.
 */
public class setMinWidth implements Testlet, PropertyChangeListener 
{
  List events = new java.util.ArrayList();
  
  public void propertyChange(PropertyChangeEvent e)
  {
    events.add(e);
  }
  
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    TableColumn c = new TableColumn();
    harness.check(c.getMinWidth(), 15);
    c.addPropertyChangeListener(this);
    
    c.setMinWidth(11);
    harness.check(c.getMinWidth(), 11);
    harness.check(events.size(), 1);
    PropertyChangeEvent e = (PropertyChangeEvent) events.get(0);
    harness.check(e.getPropertyName(), "minWidth");
    harness.check(e.getOldValue(), new Integer(15));
    harness.check(e.getNewValue(), new Integer(11));
    
    // spec says that current and preferred width will be updated if they are 
    // less than the min width
    events.clear();
    harness.check(c.getWidth(), 75);
    harness.check(c.getPreferredWidth(), 75);
    c.setMinWidth(88);
    harness.check(c.getMinWidth(), 88);
    harness.check(c.getWidth(), 88);
    harness.check(c.getPreferredWidth(), 88);
    
    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(0);
    harness.check(e1.getPropertyName(), "width");
    harness.check(e1.getOldValue(), new Integer(75));
    harness.check(e1.getNewValue(), new Integer(88));
    
    PropertyChangeEvent e2 = (PropertyChangeEvent) events.get(1);
    harness.check(e2.getPropertyName(), "preferredWidth");
    harness.check(e2.getOldValue(), new Integer(75));
    harness.check(e2.getNewValue(), new Integer(88));

    PropertyChangeEvent e3 = (PropertyChangeEvent) events.get(2);
    harness.check(e3.getPropertyName(), "minWidth");
    harness.check(e3.getOldValue(), new Integer(11));
    harness.check(e3.getNewValue(), new Integer(88));

    // the spec doesn't say anything about the max value, unfortunately
    
    // try a negative value
    c.setMinWidth(-1);
    harness.check(c.getMinWidth(), 0);
  }

}
