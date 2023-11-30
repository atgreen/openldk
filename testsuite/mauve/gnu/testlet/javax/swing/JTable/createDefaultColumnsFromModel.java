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
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

/**
 * Some checks for the createDefaultColumnsFromModel() method in the 
 * {@link JTable} class.
 */
public class createDefaultColumnsFromModel implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)       
  {
    test1(harness);
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test1(TestHarness harness)       
  {
    harness.checkPoint("Test 1");
    DefaultTableModel tm = new DefaultTableModel(2, 3);
    tm.setColumnIdentifiers(new String[] {"C0", "C1", "C2"});
    JTable table = new JTable(tm);
    
    // configure a TableColumnModel which will then be updated
    DefaultTableColumnModel tcm1 = new DefaultTableColumnModel();
    tcm1.addColumn(new TableColumn(1, 50));
    table.setColumnModel(tcm1);
    
    table.createDefaultColumnsFromModel();
    TableColumnModel tcm2 = table.getColumnModel();
    harness.check(tcm1 == tcm2);
    harness.check(tcm2.getColumnCount(), 3);
    TableColumn c0 = tcm2.getColumn(0);
    TableColumn c1 = tcm2.getColumn(1);
    TableColumn c2 = tcm2.getColumn(2);
    harness.check(c0.getIdentifier(), "C0");
    harness.check(c0.getWidth(), 75);
    harness.check(c0.getMinWidth(), 15);
    harness.check(c0.getMaxWidth(), Integer.MAX_VALUE);
    harness.check(c1.getIdentifier(), "C1");
    harness.check(c1.getWidth(), 75);
    harness.check(c1.getMinWidth(), 15);
    harness.check(c1.getMaxWidth(), Integer.MAX_VALUE);
    harness.check(c2.getIdentifier(), "C2");
    harness.check(c2.getWidth(), 75);
    harness.check(c2.getMinWidth(), 15);
    harness.check(c2.getMaxWidth(), Integer.MAX_VALUE);
  }
}

