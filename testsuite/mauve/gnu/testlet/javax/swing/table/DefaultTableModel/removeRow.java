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
 * Some tests for the removeRow() method in the {@link DefaultTableModel} class.
 */
public class removeRow implements Testlet
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
    DefaultTableModel m1 = new DefaultTableModel(3, 1);
    m1.setValueAt("V1", 0, 0);
    m1.setValueAt("V2", 1, 0);
    m1.setValueAt("V3", 2, 0);
    m1.removeRow(1);
    harness.check(m1.getRowCount(), 2);          // 1
    harness.check(m1.getValueAt(0, 0), "V1");    // 2
    harness.check(m1.getValueAt(1, 0), "V3");    // 3
  
    boolean pass = false;
    try 
    {
      m1.removeRow(-1);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);                         // 4
    
    pass = false;
    try 
    {
      m1.removeRow(99);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);                         // 5
  }

  public void testEvents(TestHarness harness) 
  {
    DefaultTableModel m1 = new DefaultTableModel(3, 1);
    MyTableModelListener listener1 = new MyTableModelListener();
    m1.addTableModelListener(listener1);
    m1.setValueAt("V1", 0, 0);
    m1.setValueAt("V2", 1, 0);
    m1.setValueAt("V3", 2, 0);
    m1.removeRow(2);
    TableModelEvent event = listener1.getEvent();
    harness.check(event.getColumn(), -1);                    // 6
    harness.check(event.getFirstRow(), 2);                   // 7
    harness.check(event.getLastRow(), 2);                    // 8
    harness.check(event.getType(), TableModelEvent.DELETE);  // 9
  }

}
