/* getAccessibleRole.java -- some checks for the getAccessibleRole() method in 
       the AccessibleJTableCell class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.2
// Uses: MyBooleanTableCellRenderer

package gnu.testlet.javax.swing.JTable.AccessibleJTable.AccessibleJTableCell;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Date;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleTable;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;

public class getAccessibleRole implements Testlet
{
  public void test(TestHarness harness)
  {
    DefaultTableModel tm = new DefaultTableModel(1, 3);
    tm.setValueAt("A", 0, 0);
    tm.setValueAt(Boolean.TRUE, 0, 1);
    tm.setValueAt(new Date(0l), 0, 2);
    JTable table = new JTable(tm);
    table.getColumnModel().getColumn(1).setCellRenderer(
            new MyBooleanTableCellRenderer());
    AccessibleContext tableac = table.getAccessibleContext();
    harness.check(tableac.getClass().getName().endsWith("AccessibleJTable"));
    AccessibleTable at = tableac.getAccessibleTable();
    Accessible accessibleCell0 = at.getAccessibleAt(0, 0);
    harness.check(accessibleCell0.getAccessibleContext().getAccessibleRole(), 
            AccessibleRole.LABEL);
    Accessible accessibleCell1 = at.getAccessibleAt(0, 1);
    harness.check(accessibleCell1.getAccessibleContext().getAccessibleRole(), 
            AccessibleRole.CHECK_BOX);
    Accessible accessibleCell2 = at.getAccessibleAt(0, 2);
    harness.check(accessibleCell2.getAccessibleContext().getAccessibleRole(), 
            AccessibleRole.LABEL);
  }
}
