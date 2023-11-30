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

package gnu.testlet.javax.swing.table.DefaultTableColumnModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.TableColumn;

/**
 * Some tests for the getColumnIndexAtX(int) method in the 
 * {@link DefaultTableColumnModel} class.
 */
public class getColumnIndexAtX implements Testlet
{

  /**
   * Runs the test using the specified harness. #
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    DefaultTableColumnModel m1 = new DefaultTableColumnModel();
    TableColumn c0 = new TableColumn(0, 5);
    TableColumn c1 = new TableColumn(1, 7);
    TableColumn c2 = new TableColumn(2, 11);
    m1.addColumn(c0);
    m1.addColumn(c1);
    m1.addColumn(c2);
    harness.check(m1.getColumnIndexAtX(-1), -1);
    harness.check(m1.getColumnIndexAtX(0), 0);
    harness.check(m1.getColumnIndexAtX(5), 1);
    harness.check(m1.getColumnIndexAtX(11), 1);
    harness.check(m1.getColumnIndexAtX(12), 2);
    harness.check(m1.getColumnIndexAtX(22), 2);
    harness.check(m1.getColumnIndexAtX(23), -1);
    
    // now repeat with a different margin - it is ignored
    m1.setColumnMargin(3);
    harness.check(m1.getColumnIndexAtX(-1), -1);
    harness.check(m1.getColumnIndexAtX(0), 0);
    harness.check(m1.getColumnIndexAtX(5), 1);
    harness.check(m1.getColumnIndexAtX(11), 1);
    harness.check(m1.getColumnIndexAtX(12), 2);
    harness.check(m1.getColumnIndexAtX(22), 2);
    harness.check(m1.getColumnIndexAtX(23), -1);
    
  }     

}
