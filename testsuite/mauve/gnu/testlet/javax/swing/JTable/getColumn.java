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
import javax.swing.table.TableColumnModel;

/**
 * Some checks for the getColumn() method in the {@link JTable} class.
 */
public class getColumn implements Testlet {

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
    tcm.getColumn(0).setIdentifier(new Integer(0));
    tcm.getColumn(1).setIdentifier(new Integer(1));
    tcm.getColumn(2).setIdentifier(new Integer(2));
    harness.check(table.getColumn(new Integer(0)).getHeaderValue(), "C1");
    harness.check(table.getColumn(new Integer(1)).getHeaderValue(), "C2");
    harness.check(table.getColumn(new Integer(2)).getHeaderValue(), "C3");
 
    boolean pass = false;
    try
    {
      /*TableColumn tc =*/ table.getColumn("XXX");
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
 
    pass = false;
    try
    {
      /*TableColumn tc =*/ table.getColumn(null);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}

