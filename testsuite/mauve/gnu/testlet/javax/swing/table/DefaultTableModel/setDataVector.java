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
 * Some tests for the setDataVector() methods in the {@link DefaultTableModel} 
 * class.
 */
public class setDataVector implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    test1(harness);
    test2(harness);
    testEvent(harness);
  }
  
  public void test1(TestHarness harness)
  {
    harness.checkPoint("setDataVector(Object[][], Object[])");
    DefaultTableModel m1 = new DefaultTableModel();
    Object[][] data = new Object[2][3];
    data[0][0] = "V1";
    data[0][1] = "V2";
    data[0][2] = "V3";
    data[1][0] = "V4";
    data[1][1] = "V5";
    data[1][2] = "V6";
    Object[] columns = new Object[] {"C1", "C2", "C3"};
    m1.setDataVector(data, columns);
    harness.check(m1.getValueAt(0, 0), "V1");
    harness.check(m1.getValueAt(0, 1), "V2");
    harness.check(m1.getValueAt(0, 2), "V3");
    harness.check(m1.getValueAt(1, 0), "V4");
    harness.check(m1.getValueAt(1, 1), "V5");
    harness.check(m1.getValueAt(1, 2), "V6");
    harness.check(m1.getColumnName(0), "C1");
    harness.check(m1.getColumnName(1), "C2");
    harness.check(m1.getColumnName(2), "C3");
    
    // do a check for uneven row lengths
    DefaultTableModel m2 = new DefaultTableModel();
    Object[][] data2 = new Object[2][];
    Object[] row1 = new Object[2];
    row1[0] = "V1";
    row1[1] = "V2";
    Object[] row2 = new Object[4];
    row2[0] = "V3";
    row2[1] = "V4";
    row2[2] = "V5";
    row2[3] = "V6";
    data2[0] = row1;
    data2[1] = row2;
    Object[] columns2 = new Object[] {"C1", "C2", "C3"};
    m2.setDataVector(data2, columns2);
    harness.check(m2.getValueAt(0, 0), "V1");
    harness.check(m2.getValueAt(0, 1), "V2");
    harness.check(m2.getValueAt(0, 2), null);
    harness.check(m2.getValueAt(1, 0), "V3");
    harness.check(m2.getValueAt(1, 1), "V4");
    harness.check(m2.getValueAt(1, 2), "V5");
    harness.check(m2.getColumnName(0), "C1");
    harness.check(m2.getColumnName(1), "C2");
    harness.check(m2.getColumnName(2), "C3");
    harness.check(m2.getColumnCount(), 3);
    harness.check(m2.getRowCount(), 2);
  }

  public void test2(TestHarness harness)
  {
    harness.checkPoint("setDataVector(Vector, Vector)");
    DefaultTableModel m1 = new DefaultTableModel();
    Vector v1 = new Vector();
    v1.add("V1");
    v1.add("V2");
    v1.add("V3");
    Vector v2 = new Vector();
    v2.add("V4");
    v2.add("V5");
    v2.add("V6");
    Vector v = new Vector();
    v.add(v1);
    v.add(v2);
    Vector columns = new Vector();
    columns.add("C1");
    columns.add("C2");
    columns.add("C3");
    m1.setDataVector(v, columns);
    harness.check(m1.getValueAt(0, 0), "V1");  // 1
    harness.check(m1.getValueAt(0, 1), "V2");  // 2
    harness.check(m1.getValueAt(0, 2), "V3");  // 3
    harness.check(m1.getValueAt(1, 0), "V4");  // 4
    harness.check(m1.getValueAt(1, 1), "V5");  // 5
    harness.check(m1.getValueAt(1, 2), "V6");  // 6
    harness.check(m1.getColumnName(0), "C1");  // 7
    harness.check(m1.getColumnName(1), "C2");  // 8
    harness.check(m1.getColumnName(2), "C3");  // 9
    
    // do a check with uneven row lengths
    DefaultTableModel m2 = new DefaultTableModel();
    Vector vv1 = new Vector();
    vv1.add("V1");
    vv1.add("V2");
    
    Vector vv2 = new Vector();
    vv2.add("V3");
    vv2.add("V4");
    vv2.add("V5");
    vv2.add("V6");
    Vector vv = new Vector();
    vv.add(vv1);
    vv.add(vv2);
    Vector columns2 = new Vector();
    columns2.add("C1");
    columns2.add("C2");
    columns2.add("C3");
    m2.setDataVector(vv, columns2);
    harness.check(m2.getValueAt(0, 0), "V1");  // 10
    harness.check(m2.getValueAt(0, 1), "V2");  // 11
    harness.check(m2.getValueAt(0, 2), null);  // 12
    harness.check(m2.getValueAt(1, 0), "V3");  // 13
    harness.check(m2.getValueAt(1, 1), "V4");  // 14
    harness.check(m2.getValueAt(1, 2), "V5");  // 15
    harness.check(m2.getColumnName(0), "C1");  // 16
    harness.check(m2.getColumnName(1), "C2");  // 17
    harness.check(m2.getColumnName(2), "C3");  // 18
    Vector data = m2.getDataVector();
    Vector r1 = (Vector) data.get(0);
    harness.check(r1.size(), 3);               // 19
    Vector r2 = (Vector) data.get(1);
    harness.check(r2.size(), 3);               // 20
    
    // test null data vector - spec says 
    // "unspecified behavior, an possibly an exception."
    // According to bug 4348070, JDK 1.3 throws an IllegalArgumentException
    // and JDK 1.4 doesn't throw any exception.
    // we check for column count matching the supplied column names which would
    // be the result of just calling setColumnIdentifiers...
    DefaultTableModel m3 = new DefaultTableModel();
    m3.setDataVector((Vector) null, columns2);
    harness.check(m3.getColumnCount(), 3);     // 21
    harness.check(m3.getDataVector().size(), 0);
    
    // test null column names - it is not specified what should happen here,
    // but we can probably assume the same behaviour as in setColumnIdentifiers,
    // which is to set a zero-column model
    DefaultTableModel m4 = new DefaultTableModel(2, 3);
    m4.setDataVector(vv, null);
    harness.check(m4.getColumnCount(), 0);     // 22
  }
  
  public void testEvent(TestHarness harness)
  {
    harness.checkPoint("Check TableModelEvent");
    DefaultTableModel m = new DefaultTableModel();
    MyTableModelListener listener = new MyTableModelListener();
    m.addTableModelListener(listener);
    Vector v1 = new Vector();
    v1.add("V1");
    v1.add("V2");
    v1.add("V3");
    Vector v2 = new Vector();
    v2.add("V4");
    v2.add("V5");
    v2.add("V6");
    Vector v = new Vector();
    v.add(v1);
    v.add(v2);
    Vector columns = new Vector();
    columns.add("C1");
    columns.add("C2");
    columns.add("C3");
    m.setDataVector(v, columns);
    TableModelEvent event = listener.getEvent();
    harness.check(event.getType(), TableModelEvent.UPDATE);
    harness.check(event.getColumn(), TableModelEvent.ALL_COLUMNS);
    harness.check(event.getFirstRow(), -1);
    harness.check(event.getLastRow(), -1);
  }
  
}
