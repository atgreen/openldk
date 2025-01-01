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

import javax.swing.table.DefaultTableModel;

/**
 * Some tests for the constructors in the {@link DefaultTableModel} class.
 */
public class constructors implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
    testConstructor5(harness);
  }

  private void testConstructor1(TestHarness harness) 
  {
    harness.checkPoint("DefaultTableModel()");
    DefaultTableModel m1 = new DefaultTableModel();
    harness.check(m1.getRowCount(), 0);
    harness.check(m1.getColumnCount(), 0);
  }

  private void testConstructor2(TestHarness harness)  
  {
    harness.checkPoint("DefaultTableModel(int, int)");
    DefaultTableModel m1 = new DefaultTableModel(2, 3);
    harness.check(m1.getRowCount(), 2);
    harness.check(m1.getColumnCount(), 3);
    harness.check(m1.getValueAt(0, 0), null);
    harness.check(m1.getValueAt(1, 2), null);
    
    DefaultTableModel m2 = new DefaultTableModel(0, 0);
    harness.check(m2.getRowCount(), 0);
    harness.check(m2.getColumnCount(), 0);

    // check negative arguments - the spec doesn't state what should
    // happen, but an IllegalArgumentException would be most likely.
    boolean pass = false;
    try 
    {
      DefaultTableModel m3 = new DefaultTableModel(-1, 1);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
  
    pass = false;
    try 
    {
      DefaultTableModel m3 = new DefaultTableModel(1, -1);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  private void testConstructor3(TestHarness harness) 
  {
    harness.checkPoint("DefaultTableModel(Object[][], Object[])"); 
    Object[][] data1 = new Object[2][3];
    Object[] columns1 = new Object[] { "C1", "C2", "C3" };
    
    // simple case
    DefaultTableModel m1 = new DefaultTableModel(data1, columns1);
    harness.check(m1.getRowCount(), 2);        // 1
    harness.check(m1.getColumnCount(), 3);     // 2
    harness.check(m1.getColumnName(0), "C1");  // 3
    harness.check(m1.getColumnName(1), "C2");  // 4
    harness.check(m1.getColumnName(2), "C3");  // 5
    
    // less column names than required
    Object[] columns2 = new Object[] { "C1" };
    DefaultTableModel m2 = new DefaultTableModel(data1, columns2);
    harness.check(m2.getColumnName(0), "C1");  // 6
    harness.check(m2.getColumnCount(), 1);     // 7

    // more column names than required
    Object[] columns3 = new Object[] { "C1", "C2", "C3", "C4" };
    DefaultTableModel m3 = new DefaultTableModel(data1, columns3);
    harness.check(m3.getColumnCount(), 4);
    harness.check(m3.getColumnName(0), "C1");  // 8
    harness.check(m3.getColumnName(1), "C2");  // 9
    harness.check(m3.getColumnName(2), "C3");  // 10
    harness.check(m3.getColumnName(3), "C4");  // 11

    // null values for column names
    Object[] columns4 = new Object[] { null, null, null };
    DefaultTableModel m4 = new DefaultTableModel(data1, columns4);
    harness.check(m4.getColumnCount(), 3);
    harness.check(m4.getColumnName(0), "A");   // 12
    harness.check(m4.getColumnName(1), "B");   // 13
    harness.check(m4.getColumnName(2), "C");   // 14
    
    // null data array - behaviour not specified
    DefaultTableModel m5 = new DefaultTableModel(null, columns1);
    harness.check(m5.getDataVector().size(), 0);
    harness.check(m5.getColumnCount(), 3);
    
    // null column names array - behaviour not specified
    DefaultTableModel m6 = new DefaultTableModel(data1, null);
    harness.check(m6.getColumnCount(), 0);
    
  }
  
  private void testConstructor4(TestHarness harness)  
  {
    harness.checkPoint("DefaultTableModel(Object[], int)");
    Object[] columns1 = new Object[] { "C1", "C2", "C3" };
    DefaultTableModel m1 = new DefaultTableModel(columns1, 2);
    harness.check(m1.getRowCount(), 2);            // check 1
    harness.check(m1.getColumnCount(), 3);         // check 2
    harness.check(m1.getColumnName(0), "C1");      // check 3
    harness.check(m1.getColumnName(1), "C2");      // check 4
    harness.check(m1.getColumnName(2), "C3");      // check 5
    
    // empty column names
    Object[] columns2 = new Object[] { };
    DefaultTableModel m2 = new DefaultTableModel(columns2, 2);
    harness.check(m2.getRowCount(), 2);            // check 6
    harness.check(m2.getColumnCount(), 0);         // check 7
    
    // null column names
    Object[] columns3 = new Object[] { "C1", null, "C3" };
    DefaultTableModel m3 = new DefaultTableModel(columns3, 2);
    harness.check(m3.getRowCount(), 2);            // check 8
    harness.check(m3.getColumnCount(), 3);         // check 9
    harness.check(m3.getColumnName(0), "C1");      // check 10
    harness.check(m3.getColumnName(1), "B");       // check 11
    harness.check(m3.getColumnName(2), "C3");      // check 12

    // null array
    Object[] columns4 = null;
    DefaultTableModel m4 = new DefaultTableModel(columns4, 2);
    harness.check(m4.getColumnCount(), 0);
    harness.check(m4.getRowCount(), 2);
    
    // negative row count
    boolean pass = false;
    try 
    {
      /*DefaultTableModel m5 =*/ new DefaultTableModel(columns1, -1);
    }
    catch (RuntimeException e) 
    {
      pass = true;    
    }
    harness.check(pass);
  }

  private void testConstructor5(TestHarness harness)  
  {
    harness.checkPoint("DefaultTableModel(Vector, int)");
    Vector columns1 = new Vector();
    columns1.add("C1");
    columns1.add("C2");
    columns1.add("C3");
    DefaultTableModel m1 = new DefaultTableModel(columns1, 2);
    harness.check(m1.getRowCount(), 2);
    harness.check(m1.getColumnCount(), 3);
    harness.check(m1.getColumnName(0), "C1");
    harness.check(m1.getColumnName(1), "C2");
    harness.check(m1.getColumnName(2), "C3");
    
    // empty column names
    Vector columns2 = new Vector();
    DefaultTableModel m2 = new DefaultTableModel(columns2, 2);
    harness.check(m2.getRowCount(), 2);
    harness.check(m2.getColumnCount(), 0);
    
    // null column names
    Vector columns3 = new Vector();
    columns3.add("C1");
    columns3.add(null);
    columns3.add("C3");
    
    DefaultTableModel m3 = new DefaultTableModel(columns3, 2);
    harness.check(m3.getRowCount(), 2);
    harness.check(m3.getColumnCount(), 3);
    harness.check(m3.getColumnName(0), "C1");
    harness.check(m3.getColumnName(1), "B");
    harness.check(m3.getColumnName(2), "C3");

    // null vector
    Vector columns4 = null;
    boolean pass = false;
    try
    {
      DefaultTableModel m4 = new DefaultTableModel(columns4, 2);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }

    // negative row count
    pass = false;
    try 
    {
      DefaultTableModel m5 = new DefaultTableModel(columns1, -1);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;    
    }
    harness.check(pass);
  }

}
