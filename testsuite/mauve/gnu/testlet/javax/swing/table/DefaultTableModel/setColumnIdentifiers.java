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

import java.util.Vector;

import javax.swing.event.TableModelEvent;
import javax.swing.table.DefaultTableModel;

/**
 * Some tests for the setColumnIdentifiers() method in the 
 * {@link DefaultTableModel} class.
 */
public class setColumnIdentifiers implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testBasics1(harness);
    testBasics2(harness);
    testEvents1(harness);
    testEvents2(harness);
  }
  
  public void testBasics1(TestHarness harness) 
  {
    harness.checkPoint("setColumnIdentifiers(Object[])");
    DefaultTableModel m1 = new DefaultTableModel(1, 3);
    m1.setColumnIdentifiers(new Object[] {"C1", "C2", "C3"});
    harness.check(m1.getColumnName(0), "C1");  // 1
    harness.check(m1.getColumnName(1), "C2");  // 2
    harness.check(m1.getColumnName(2), "C3");  // 3
    Vector v1 = m1.getDataVector();
    Vector v1a = (Vector) v1.get(0);
    harness.check(v1a.size(), 3);              // 4

    m1.setColumnIdentifiers(new Object[] {"SINGLE COLUMN"});
    harness.check(m1.getColumnCount(), 1);                // 5
    harness.check(m1.getColumnName(0), "SINGLE COLUMN");  // 6
    Vector v2 = m1.getDataVector();
    Vector v2a = (Vector) v2.get(0);
    harness.check(v2a.size(), 1);                         // 7
    
    m1.setColumnIdentifiers((Object[]) null);
    harness.check(m1.getColumnCount(), 0);                // 8
    Vector v3 = m1.getDataVector();
    Vector v3a = (Vector) v3.get(0);
    harness.check(v3a.size(), 0);
  }

  public void testBasics2(TestHarness harness) 
  {
    harness.checkPoint("setColumnIdentifiers(Vector)");
    DefaultTableModel m1 = new DefaultTableModel(1, 3);
    Vector v = new Vector();
    v.add("C1");
    v.add("C2");
    v.add("C3");
    m1.setColumnIdentifiers(v);
    harness.check(m1.getColumnName(0), "C1");  // 1
    harness.check(m1.getColumnName(1), "C2");  // 2
    harness.check(m1.getColumnName(2), "C3");  // 3
    Vector v1 = m1.getDataVector();
    Vector v1a = (Vector) v1.get(0);
    harness.check(v1a.size(), 3);              // 4

    Vector cols2 = new Vector();
    cols2.add("SINGLE COLUMN");
    m1.setColumnIdentifiers(cols2);
    harness.check(m1.getColumnCount(), 1);                // 5
    harness.check(m1.getColumnName(0), "SINGLE COLUMN");  // 6
    Vector v2 = m1.getDataVector();
    Vector v2a = (Vector) v2.get(0);
    harness.check(v2a.size(), 1);                         // 7
    
    m1.setColumnIdentifiers((Vector) null);
    harness.check(m1.getColumnCount(), 0);                // 8
    Vector v3 = m1.getDataVector();
    Vector v3a = (Vector) v3.get(0);
    harness.check(v3a.size(), 0);
  }

  public void testEvents1(TestHarness harness) 
  {
    DefaultTableModel m1 = new DefaultTableModel(1, 3);
    MyTableModelListener listener1 = new MyTableModelListener();
    m1.addTableModelListener(listener1);
    m1.setColumnIdentifiers(new Object[] {"C1", "C2", "C3"});
    TableModelEvent event = listener1.getEvent();
    harness.check(event.getType(), TableModelEvent.UPDATE);
    harness.check(event.getColumn(), TableModelEvent.ALL_COLUMNS);
    harness.check(event.getFirstRow(), -1);
    harness.check(event.getLastRow(), -1);
  }

  public void testEvents2(TestHarness harness) 
  {
    DefaultTableModel m1 = new DefaultTableModel(1, 3);
    Vector v = new Vector();
    v.add("C1");
    v.add("C2");
    v.add("C3");
    MyTableModelListener listener1 = new MyTableModelListener();
    m1.addTableModelListener(listener1);
    m1.setColumnIdentifiers(v);
    TableModelEvent event = listener1.getEvent();
    harness.check(event.getType(), TableModelEvent.UPDATE);
    harness.check(event.getColumn(), TableModelEvent.ALL_COLUMNS);
    harness.check(event.getFirstRow(), -1);
    harness.check(event.getLastRow(), -1);
  }

}
