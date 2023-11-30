// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

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

import javax.swing.JTable;
import javax.swing.ListSelectionModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests the functionality of the method isRowSelected in JTable. This test
 * shows that isRowSelected() returns true exactly when the requested row index
 * is set as selected in the table's selectionModel, independent of the
 * rowSelectionAllowed property.
 * 
 * @author Roman Kennke (kennke@aicas.com)
 */
public class isRowSelected implements Testlet
{

  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testRowSelectionAllowed(harness);
    testRowSelectionNotAllowed(harness);
  }

  /**
   * Tests the case when the rowSelectionAllowed property is true.
   *
   * @param harness the test harness to use
   */
  private void testRowSelectionAllowed(TestHarness harness)
  {
    JTable table = createTestTable();
    table.setRowSelectionAllowed(true);
    ListSelectionModel selModel = table.getSelectionModel();
    // Select row#1 in the selection model.
    selModel.setSelectionInterval(1, 1);
    harness.check(table.isRowSelected(1), true);
  }

  /**
   * Tests the case when the rowSelectionAllowed property is false.
   *
   * @param harness the test harness to use
   */
  private void testRowSelectionNotAllowed(TestHarness harness)
  {
    JTable table = createTestTable();
    table.setRowSelectionAllowed(false);
    ListSelectionModel selModel = table.getSelectionModel();
    // Select row#1 in the selection model.
    selModel.setSelectionInterval(1, 1);
    harness.check(table.isRowSelected(1), true);
  }

  /**
   * Creates a JTable used for testing.
   *
   * @return the test table
   */
  private JTable createTestTable()
  {
    return new JTable(new Object[][] {{"1", "2", "3"}, {"4", "5", "6"},
                                      {"7", "8", "9"}},
                      new Object[] {"1", "2", "3"});
  }
}
