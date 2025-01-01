// Tags: JDK1.2
// Uses: MyTableModel ../DefaultTableModel/MyTableModelListener

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

package gnu.testlet.javax.swing.table.AbstractTableModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.javax.swing.table.DefaultTableModel.MyTableModelListener;

import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;

/**
 * Some tests for the fireTableCellUpdated() method in the 
 * {@link AbstractTableModel} class.
 */
public class fireTableCellUpdated implements Testlet
{

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    MyTableModelListener listener = new MyTableModelListener();
    MyTableModel m = new MyTableModel();
    m.addTableModelListener(listener);
    m.fireTableCellUpdated(2, 3);
    TableModelEvent e = listener.getEvent();
    harness.check(e.getFirstRow(), 2);
    harness.check(e.getLastRow(), 2);
    harness.check(e.getColumn(), 3);
    harness.check(e.getSource(), m);
    harness.check(e.getType(), TableModelEvent.UPDATE);
  }

}
