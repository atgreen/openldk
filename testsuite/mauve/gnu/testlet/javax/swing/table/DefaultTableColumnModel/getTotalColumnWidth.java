/* getTotalColumnWidth.java -- some checks for the getTotalColumnWidth() method
       in the DefaultTableColumnModel class.
   Copyright (C) 2006 by David Gilbert <david.gilbert@object-refinery.com>
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

package gnu.testlet.javax.swing.table.DefaultTableColumnModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.TableColumn;

public class getTotalColumnWidth implements Testlet
{
  public void test(TestHarness harness)
  {
    DefaultTableColumnModel m = new DefaultTableColumnModel();
    harness.check(m.getTotalColumnWidth(), 0);
    TableColumn c1 = new TableColumn(0, 9);
    m.addColumn(c1);
    harness.check(m.getTotalColumnWidth(), 9);
    TableColumn c2 = new TableColumn(1, 12);
    m.addColumn(c2);
    harness.check(m.getTotalColumnWidth(), 21);
    
    // column margin is not included
    m.setColumnMargin(5);
    harness.check(m.getTotalColumnWidth(), 21);
    
    c1.setWidth(99);
    harness.check(m.getTotalColumnWidth(), 111);
  }
}
