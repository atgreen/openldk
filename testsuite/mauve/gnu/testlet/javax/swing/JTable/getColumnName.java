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

package gnu.testlet.javax.swing.JTable;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

/**
 * Some checks for the getColumnName() method in the {@link JTable} class.
 */
public class getColumnName implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    DefaultTableModel tm = new DefaultTableModel(2, 3);
    tm.setColumnIdentifiers(new String[] {"C1", "C2", "C3"});
    JTable table = new JTable(tm);
    TableColumnModel tcm = table.getColumnModel();
    TableColumn c0 = tcm.getColumn(0);
    TableColumn c1 = tcm.getColumn(1);
    c0.setModelIndex(1);
    c1.setModelIndex(0);
    harness.check(table.getColumnName(0), "C2");
    harness.check(table.getColumnName(1), "C1");
    harness.check(table.getColumnName(2), "C3");
    
    c0.setHeaderValue("XX");
    harness.check(table.getColumnName(0), "C2");
    c0.setIdentifier("XXX");
    harness.check(table.getColumnName(0), "C2");
    
    boolean pass = false;
    try
    {
      /*String s =*/ table.getColumnName(-1);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      /*String s =*/ table.getColumnName(3);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}

