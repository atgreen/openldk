// Tags: JDK1.2

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

package gnu.testlet.javax.swing.table.DefaultTableModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.event.TableModelEvent;
import javax.swing.table.DefaultTableModel;

/**
 * Some tests for the setRowCount() method in the {@link DefaultTableModel} 
 * class.
 */
public class setRowCount implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testBasics(harness);
    testEvents(harness);
  }
  
  public void testBasics(TestHarness harness) 
  {
    DefaultTableModel m1 = new DefaultTableModel(
            new Object[] {"C1", "C2", "C3"}, 3);
    m1.setValueAt("V1", 0, 0);
    m1.setValueAt("V2", 1, 0);
    m1.setValueAt("V3", 2, 0);
    m1.setRowCount(4);
    harness.check(m1.getRowCount(), 4);
    harness.check(m1.getValueAt(3, 0), null);
  
    DefaultTableModel m2 = new DefaultTableModel(3, 1);
    m2.setValueAt("V1", 0, 0);
    m2.setValueAt("V2", 1, 0);
    m2.setValueAt("V3", 2, 0);
    m2.setRowCount(4);
    harness.check(m2.getRowCount(), 4);
    harness.check(m2.getValueAt(0, 0), "V1");
  
    // negative row count - the spec doesn't say what exception this should
    // throw, so we'll just check for a runtime exception
    DefaultTableModel m3 = new DefaultTableModel();
    boolean pass = false;
    try 
    {
      m3.setRowCount(-1);
    }
    catch (RuntimeException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

  public void testEvents(TestHarness harness) 
  {
    DefaultTableModel m1 = new DefaultTableModel(3, 1);
    MyTableModelListener listener1 = new MyTableModelListener();
    m1.addTableModelListener(listener1);
    m1.setValueAt("V1", 0, 0);
    m1.setValueAt("V2", 1, 0);
    m1.setValueAt("V3", 2, 0);
    m1.setRowCount(5);
    TableModelEvent event = listener1.getEvent();
    harness.check(event.getType(), TableModelEvent.INSERT);
    harness.check(event.getColumn(), TableModelEvent.ALL_COLUMNS);
    harness.check(event.getFirstRow(), 3);
    harness.check(event.getLastRow(), 4);

    DefaultTableModel m2 = new DefaultTableModel(3, 1);
    MyTableModelListener listener2 = new MyTableModelListener();
    m2.addTableModelListener(listener2);
    m2.setValueAt("V1", 0, 0);
    m2.setValueAt("V2", 1, 0);
    m2.setValueAt("V3", 2, 0);
    m2.setRowCount(1);
    TableModelEvent event2 = listener2.getEvent();
    harness.check(event2.getType(), TableModelEvent.DELETE);
    harness.check(event2.getColumn(), TableModelEvent.ALL_COLUMNS);
    harness.check(event2.getFirstRow(), 1);
    harness.check(event2.getLastRow(), 2);
  }

}
