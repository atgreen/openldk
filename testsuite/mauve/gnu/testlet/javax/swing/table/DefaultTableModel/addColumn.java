// Tags: JDK1.2
// Uses: MyTableModelListener

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

import java.util.Vector;

import javax.swing.event.TableModelEvent;
import javax.swing.table.DefaultTableModel;

/**
 * Some tests for the addColumn() methods in the {@link DefaultTableModel} 
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
    testAddColumn1(harness);
    testAddColumn2(harness);
    testAddColumn3(harness);
  }

  private void testAddColumn1(TestHarness harness) 
  {
    harness.checkPoint("addColumn(Object)");
    DefaultTableModel m1 = new DefaultTableModel();
    MyTableModelListener listener1 = new MyTableModelListener();
    m1.addTableModelListener(listener1);
    m1.addColumn("C1");
    harness.check(m1.getColumnCount(), 1);                  // 1
    harness.check(m1.getColumnName(0), "C1");               // 2 
    TableModelEvent event = listener1.getEvent();
    harness.check(event.getType(), TableModelEvent.UPDATE); // 3
    harness.check(event.getColumn(), TableModelEvent.ALL_COLUMNS); 
    harness.check(event.getFirstRow(), -1);
    harness.check(event.getLastRow(), -1);
    
    // null argument is permitted in JDK 1.4 (see bug report 4474094)
    boolean pass = true;
    try 
    {
      m1.addColumn(null);
    }
    catch (IllegalArgumentException e) 
    {
      pass = false;
    }
    harness.check(pass);                                    
  }

  private void testAddColumn2(TestHarness harness) 
  {
    harness.checkPoint("addColumn(Object, Vector)");
    
    // correct number of data values
    DefaultTableModel m1 = new DefaultTableModel(2, 3);
    MyTableModelListener listener1 = new MyTableModelListener();
    m1.addTableModelListener(listener1);
    Vector columnData1 = new Vector();
    columnData1.add("V1");
    columnData1.add("V2");
    m1.addColumn("C4", columnData1);
    harness.check(m1.getColumnCount(), 4);                 // 1
    harness.check(m1.getColumnName(3), "C4");              // 2
    harness.check(m1.getValueAt(0, 3), "V1");              // 3
    harness.check(m1.getValueAt(1, 3), "V2");              // 4
    TableModelEvent event = listener1.getEvent();
    harness.check(event.getType(), TableModelEvent.UPDATE); // 5
    harness.check(event.getColumn(), TableModelEvent.ALL_COLUMNS); // 6 
    harness.check(event.getFirstRow(), -1);                 // 7
    harness.check(event.getLastRow(), -1);                  // 8
    
    // too few data values
    DefaultTableModel m2 = new DefaultTableModel(2, 3);
    MyTableModelListener listener2 = new MyTableModelListener();
    m2.addTableModelListener(listener2);
    Vector columnData2 = new Vector();
    columnData2.add("V1");
    m2.addColumn("C4", columnData2);
    harness.check(m2.getColumnCount(), 4);                 // 9
    harness.check(m2.getColumnName(3), "C4");              // 10
    harness.check(m2.getValueAt(0, 3), "V1");              // 11
    harness.check(m2.getValueAt(1, 3), null);              // 12
    event = listener2.getEvent();
    harness.check(event.getType(), TableModelEvent.UPDATE); 
    harness.check(event.getColumn(), TableModelEvent.ALL_COLUMNS); 
    harness.check(event.getFirstRow(), -1);
    harness.check(event.getLastRow(), -1);
    
    // too many data values
    DefaultTableModel m3 = new DefaultTableModel(
            new Object[] {"C1", "C2", "C3"}, 2);
    Vector columnData3 = new Vector();
    columnData3.add("V1");
    columnData3.add("V2");
    columnData3.add("V3");
    m3.addColumn("C4", columnData3);
    harness.check(m3.getColumnCount(), 4);                 // 13
    harness.check(m3.getColumnName(3), "C4");              // 14
    harness.check(m3.getValueAt(0, 3), "V1");              // 15
    harness.check(m3.getValueAt(1, 3), "V2");              // 16
    
    harness.check(m3.getValueAt(2, 3), "V3");              // 17
    harness.check(m3.getValueAt(2, 2), null);              // 18
    
    // null column name argument is permitted in JDK 1.4 
    // (see bug report 4474094)
    boolean pass = true;
    try 
    {
      m1.addColumn(null, new Vector());
    }
    catch (RuntimeException e) 
    {
      pass = false;
    }
    harness.check(pass);
    
    // null data values
    DefaultTableModel m4 = new DefaultTableModel();
    m4.addColumn("C1", (Vector) null);
    harness.check(m4.getColumnName(0), "C1");
    harness.check(m4.getRowCount(), 0);
  }

  private void testAddColumn3(TestHarness harness) 
  {
    harness.checkPoint("addColumn(Object, Object[])");
    
    // correct number of data values
    DefaultTableModel m1 = new DefaultTableModel(2, 3);
    Object[] columnData1 = new Object[2];
    columnData1[0] = "V1";
    columnData1[1] = "V2";
    m1.addColumn("C4", columnData1);
    harness.check(m1.getColumnCount(), 4);                 // 1
    harness.check(m1.getColumnName(3), "C4");              // 2
    harness.check(m1.getValueAt(0, 3), "V1");              // 3
    harness.check(m1.getValueAt(1, 3), "V2");              // 4
    
    // too few data values
    DefaultTableModel m2 = new DefaultTableModel(2, 3);
    Object[] columnData2 = new Object[1];
    columnData2[0] = "V1";
    m2.addColumn("C4", columnData2);
    harness.check(m2.getColumnCount(), 4);                 // 5
    harness.check(m2.getColumnName(3), "C4");              // 6
    harness.check(m2.getValueAt(0, 3), "V1");              // 7
    harness.check(m2.getValueAt(1, 3), null);              // 8
    
    // too many data values
    DefaultTableModel m3 = new DefaultTableModel(
            new Object[] {"C1", "C2", "C3"}, 2);
    Object[] columnData3 = new Object[3];
    columnData3[0] = "V1";
    columnData3[1] = "V2";
    columnData3[2] = "V3";
    m3.addColumn("C4", columnData3);
    harness.check(m3.getColumnCount(), 4);                 // 9
    harness.check(m3.getColumnName(3), "C4");              // 10
    harness.check(m3.getValueAt(0, 3), "V1");              // 11
    harness.check(m3.getValueAt(1, 3), "V2");              // 12
    harness.check(m3.getValueAt(2, 3), "V3");              // 13
    harness.check(m3.getValueAt(2, 2), null);              // 14
    
    // null column name argument is permitted in JDK 1.4 
    // (see bug report 4474094)
    boolean pass = true;
    try 
    {
      m1.addColumn(null, new Vector());
    }
    catch (RuntimeException e) 
    {
      pass = false;
    }
    harness.check(pass);
    
    // null data values
    DefaultTableModel m4 = new DefaultTableModel();
    m4.addColumn("C1", (Object[]) null);
    harness.check(m4.getColumnName(0), "C1");
    harness.check(m4.getRowCount(), 0);
  }

}
