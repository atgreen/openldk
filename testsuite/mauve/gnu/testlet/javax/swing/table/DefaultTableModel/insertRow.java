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
 * Some tests for the insertRow() methods in the {@link DefaultTableModel} 
 * class.
 */
public class insertRow implements Testlet
{
  
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testInsertRow1(harness);
    testInsertRow2(harness);
  }

  private void testInsertRow1(TestHarness harness) 
  {
    harness.checkPoint("insertRow(int, Object[])");
    DefaultTableModel m1 = new DefaultTableModel(
            new Object[] {"C1", "C2", "C3"}, 0);
    MyTableModelListener listener1 = new MyTableModelListener();
    m1.addTableModelListener(listener1);
    m1.insertRow(0, new Object[] {"V1", "V2", "V3"});
    harness.check(m1.getColumnCount(), 3);
    harness.check(m1.getRowCount(), 1);
    TableModelEvent event = listener1.getEvent();
    harness.check(event.getType(), TableModelEvent.INSERT);
    listener1.setEvent(null);
    
    m1.insertRow(0, (Object[]) null);
    harness.check(m1.getRowCount(), 2);
    event = listener1.getEvent();
    harness.check(event.getType(), TableModelEvent.INSERT);
    harness.check(event.getColumn(), TableModelEvent.ALL_COLUMNS); 
    harness.check(event.getFirstRow(), 0);
    harness.check(event.getLastRow(), 0);
    listener1.setEvent(null);
    
    // negative row index
    boolean pass = false;
    try
    {
      m1.insertRow(-1, (Object[]) null);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);

    // row index too large
    pass = false;
    try
    {
      m1.insertRow(999, (Object[]) null);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);
}

  private void testInsertRow2(TestHarness harness) 
  {
    harness.checkPoint("insertRow(int, Vector)");
    Vector v1 = new Vector();
    v1.add("V1");
    v1.add("V2");
    v1.add("V3");
    DefaultTableModel m1 = new DefaultTableModel(
            new Object[] {"C1", "C2", "C3"}, 0);
    MyTableModelListener listener1 = new MyTableModelListener();
    m1.addTableModelListener(listener1);
    m1.insertRow(0, v1);
    harness.check(m1.getColumnCount(), 3);
    harness.check(m1.getRowCount(), 1);
    TableModelEvent event = listener1.getEvent();
    harness.check(event.getType(), TableModelEvent.INSERT);
    harness.check(event.getColumn(), TableModelEvent.ALL_COLUMNS); 
    harness.check(event.getFirstRow(), 0);
    harness.check(event.getLastRow(), 0);
    listener1.setEvent(null);
    
    m1.insertRow(0, (Vector) null);
    harness.check(m1.getRowCount(), 2);
    event = listener1.getEvent();
    harness.check(event.getType(), TableModelEvent.INSERT);
    harness.check(event.getColumn(), TableModelEvent.ALL_COLUMNS); 
    harness.check(event.getFirstRow(), 0);
    harness.check(event.getLastRow(), 0);
    listener1.setEvent(null);
    
    // negative row index
    boolean pass = false;
    try
    {
      m1.insertRow(-1, (Vector) null);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);

    // row index too large
    pass = false;
    try
    {
      m1.insertRow(999, (Vector) null);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

}
