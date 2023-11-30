/* getForeground.java -- some checks for the getForeground() method
       in the AccessibleJTableHeaderEntry class.
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
// Uses: MyTableCellRenderer

package gnu.testlet.javax.swing.table.JTableHeader.AccessibleJTableHeader.AccessibleJTableHeaderEntry;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleComponent;
import javax.accessibility.AccessibleContext;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.JTableHeader;


public class getForeground implements Testlet
{
  public void test(TestHarness harness)
  {
    DefaultTableModel tm = new DefaultTableModel(new String[] {"AA", "BB", "CC"}, 3);
    JTable t = new JTable(tm);
    JTableHeader th = t.getTableHeader();
    AccessibleContext ac = th.getAccessibleContext();
    Accessible ac0 = ac.getAccessibleChild(0);
    harness.check(ac0.getClass().getName().endsWith("AccessibleJTableHeaderEntry"));
    AccessibleComponent ac0ac = ac0.getAccessibleContext().getAccessibleComponent();
    JLabel label = new JLabel("Just to get the foreground");
    harness.check(ac0ac.getForeground(), label.getForeground());
  
    MyTableCellRenderer r = new MyTableCellRenderer();
    r.setForeground(Color.yellow);
    t.getColumnModel().getColumn(1).setHeaderRenderer(r);
    Accessible ac1 = ac.getAccessibleChild(1);
    harness.check(ac1.getClass().getName().endsWith("AccessibleJTableHeaderEntry"));
    AccessibleComponent ac1ac = ac1.getAccessibleContext().getAccessibleComponent();
    harness.check(ac1ac.getForeground(), Color.yellow);
  }
}
