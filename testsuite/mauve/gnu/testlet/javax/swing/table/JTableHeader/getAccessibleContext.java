/* getAccessibleContext.java -- some checks for the getAccessibleContext()
       method in the JTableHeader class.
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

package gnu.testlet.javax.swing.table.JTableHeader;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.accessibility.AccessibleComponent;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.JTable;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;

public class getAccessibleContext implements Testlet
{
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
  }
  
  public void test1(TestHarness harness)
  {
    JTableHeader th = new JTableHeader();
    AccessibleContext ac = th.getAccessibleContext();
    harness.check(ac.getAccessibleName(), null);
    harness.check(ac.getAccessibleRole(), AccessibleRole.PANEL);
    harness.check(ac.getAccessibleChildrenCount(), 0);
    harness.check(ac.getAccessibleChild(0), null);
    
    AccessibleComponent acomp = ac.getAccessibleComponent();
    harness.check(acomp.getBackground(), th.getBackground());
  }
  
  public void test2(TestHarness harness)
  {
    harness.checkPoint("test2");
    DefaultTableModel tm = new DefaultTableModel(2, 3);
    DefaultTableColumnModel tcm = new DefaultTableColumnModel();
    TableColumn tc0 = new TableColumn(0, 10);
    tc0.setHeaderValue("XYZ0");
    tcm.addColumn(tc0);
    tcm.addColumn(new TableColumn(1, 20));
    tcm.addColumn(new TableColumn(2, 30));
    JTable t = new JTable(tm, tcm);
    JTableHeader th = t.getTableHeader();
    AccessibleContext ac = th.getAccessibleContext();
    harness.check(ac.getAccessibleName(), null);
    harness.check(ac.getAccessibleRole(), AccessibleRole.PANEL);
    harness.check(ac.getAccessibleChildrenCount(), 3);

    AccessibleContext he0 = ac.getAccessibleChild(0).getAccessibleContext();
    harness.check(he0.getAccessibleName(), "XYZ0");
    harness.check(he0.getAccessibleRole(), AccessibleRole.LABEL);
    harness.check(he0.getAccessibleComponent(), he0);
    
  }
}
