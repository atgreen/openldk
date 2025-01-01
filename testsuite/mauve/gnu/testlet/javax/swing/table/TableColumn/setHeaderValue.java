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

import javax.swing.table.TableColumn;

/**
 * Some tests for the setHeaderValue() method in the {@link TableColumn} class.
 */
public class setHeaderValue implements Testlet, PropertyChangeListener
{

  private PropertyChangeEvent event;
  
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    TableColumn c = new TableColumn();
    c.setHeaderValue(new Integer(99));
    harness.check(c.getHeaderValue(), new Integer(99));
    
    // O'Reilly's "Java Swing" (first edition) lists this as a "bound" property
    c.addPropertyChangeListener(this);
    c.setHeaderValue("Value");
    harness.check(event.getPropertyName(), "headerValue");
    harness.check(event.getOldValue(), new Integer(99));
    harness.check(event.getNewValue(), "Value");
    
    c.setHeaderValue(null);
    harness.check(c.getHeaderValue(), null);
    harness.check(event.getPropertyName(), "headerValue");
    harness.check(event.getOldValue(), "Value");
    harness.check(event.getNewValue(), null);
  }
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    this.event = e;
  }

}
