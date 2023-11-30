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

package gnu.testlet.javax.swing.table.DefaultTableModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.event.TableModelEvent;
import javax.swing.table.DefaultTableModel;

/**
 * Some tests for the moveRow() methods in the {@link DefaultTableModel} class.
 */
public class moveRow implements Testlet
{
  
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    Object[][] data = new Object[][] {{"A"}, {"B"}, {"C"}, {"D"}, {"E"}, {"F"},
            {"G"}, {"H"}, {"I"}, {"J"}, {"K"}};
    DefaultTableModel m1 = new DefaultTableModel(data, new Object[] {"C1"});
    MyTableModelListener listener1 = new MyTableModelListener();
    m1.addTableModelListener(listener1);
    m1.moveRow(1, 3, 5);
    harness.check(m1.getValueAt(0, 0), "A");
    harness.check(m1.getValueAt(1, 0), "E");
    harness.check(m1.getValueAt(2, 0), "F");
    harness.check(m1.getValueAt(3, 0), "G");
    harness.check(m1.getValueAt(4, 0), "H");
    harness.check(m1.getValueAt(5, 0), "B");
    harness.check(m1.getValueAt(6, 0), "C");
    harness.check(m1.getValueAt(7, 0), "D");
    harness.check(m1.getValueAt(8, 0), "I");
    harness.check(m1.getValueAt(9, 0), "J");
    harness.check(m1.getValueAt(10, 0), "K");
    TableModelEvent event = listener1.getEvent();
    harness.check(event.getType(), TableModelEvent.UPDATE);
    harness.check(event.getColumn(), TableModelEvent.ALL_COLUMNS); 
    harness.check(event.getFirstRow(), 1);
    harness.check(event.getLastRow(), 7);
    listener1.setEvent(null);

    DefaultTableModel m2 = new DefaultTableModel(data, new Object[] {"C1"});
    m2.moveRow(6, 7, 1);
    harness.check(m2.getValueAt(0, 0), "A");
    harness.check(m2.getValueAt(1, 0), "G");
    harness.check(m2.getValueAt(2, 0), "H");
    harness.check(m2.getValueAt(3, 0), "B");
    harness.check(m2.getValueAt(4, 0), "C");
    harness.check(m2.getValueAt(5, 0), "D");
    harness.check(m2.getValueAt(6, 0), "E");
    harness.check(m2.getValueAt(7, 0), "F");
    harness.check(m2.getValueAt(8, 0), "I");
    harness.check(m2.getValueAt(9, 0), "J");
    harness.check(m2.getValueAt(10, 0), "K");
  }

}
