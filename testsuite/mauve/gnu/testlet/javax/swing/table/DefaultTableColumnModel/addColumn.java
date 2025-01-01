// Tags: JDK1.2
// Uses: MyListener

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.table.DefaultTableColumnModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeListener;

import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.TableColumn;

/**
 * Some tests for the addColumn() method in the {@link DefaultTableColumnModel}
 * class.
 */
public class addColumn implements Testlet
{
  
  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    DefaultTableColumnModel m1 = new DefaultTableColumnModel();
    TableColumn c1 = new TableColumn(1, 23);
    m1.addColumn(c1);
    harness.check(m1.getColumnCount(), 1);
    PropertyChangeListener[] listeners = c1.getPropertyChangeListeners();
    harness.check(listeners[0], m1);
    
    TableColumn c = m1.getColumn(0);
    harness.check(c.getWidth(), 23);

    boolean pass = false;
    try
    {
      m1.addColumn(null);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // check that adding a column sends the correct event
    MyListener listener = new MyListener();
    m1.addColumnModelListener(listener);
    m1.addColumn(new TableColumn(2, 45));
    harness.check(listener.getEvent() != null);
    harness.check(listener.getEvent().getFromIndex(), 0);
    harness.check(listener.getEvent().getToIndex(), 1);
  }
  
}
