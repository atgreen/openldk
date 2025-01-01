// Tags: JDK1.2
// Uses: MyListener

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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.table.DefaultTableColumnModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.TableColumn;

/**
 * Some tests for the removeColumn() method in the 
 * {@link DefaultTableColumnModel} class.
 */
public class removeColumn implements Testlet
{

  /**
   * Runs the test using the specified harness. 
   *  
   * @param harness  the test harness (<code>null</code> not permitted).
   */ 
  public void test(TestHarness harness)      
  {
    DefaultTableColumnModel m1 = new DefaultTableColumnModel();
    TableColumn c0 = new TableColumn(0, 23);
    TableColumn c1 = new TableColumn(1, 34);
    TableColumn c2 = new TableColumn(2, 45);
    m1.addColumn(c0);
    m1.addColumn(c1);
    m1.addColumn(c2);

    m1.removeColumn(c1);
    harness.check(m1.getColumnCount(), 2);
    harness.check(c1.getPropertyChangeListeners().length, 0);
    
    // remove a column that isn't there
    TableColumn c3 = new TableColumn(3, 99);
    m1.removeColumn(c3);
    harness.check(m1.getColumnCount(), 2);

    // remove a null column
    m1.removeColumn(null);
    harness.check(m1.getColumnCount(), 2);
 
    // check that removing a column sends the correct event
    MyListener listener = new MyListener();
    m1.addColumnModelListener(listener);
    m1.removeColumn(c0);
    harness.check(listener.getEvent() != null);
    harness.check(listener.getEvent().getFromIndex(), 0);
    harness.check(listener.getEvent().getToIndex(), 0);
  }

}
