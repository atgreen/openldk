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
 * Some checks for the convertColumnIndexToView() method in the {@link JTable} 
 * class.
 */
public class convertColumnIndexToView implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("convertColumnIndexToView()");
    DefaultTableModel m1 = new DefaultTableModel(4, 6);
    JTable t = new JTable(m1);
    harness.check(t.convertColumnIndexToView(0), 0);
    harness.check(t.convertColumnIndexToView(-1), -1);
    harness.check(t.convertColumnIndexToView(6), -1);
    harness.check(t.convertColumnIndexToView(999), -1);

    TableColumnModel tcm = t.getColumnModel();
    tcm.moveColumn(0, 4);
    harness.check(t.convertColumnIndexToView(0), 4);
    harness.check(t.convertColumnIndexToView(1), 0);
    harness.check(t.convertColumnIndexToView(2), 1);
    harness.check(t.convertColumnIndexToView(3), 2);
    harness.check(t.convertColumnIndexToView(4), 3);
    harness.check(t.convertColumnIndexToView(5), 5);
  }
}