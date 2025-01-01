// Tags: JDK1.2
// Uses: MyDefaultTableColumnModel

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

import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import javax.swing.table.DefaultTableColumnModel;

/**
 * Some tests for the constructor in the {@link DefaultTableColumnModel} class.
 */
public class constructor implements Testlet, TableColumnModelListener 
{

  public void columnAdded(TableColumnModelEvent e) 
  {
    // ignore  
  }

  public void columnMarginChanged(ChangeEvent e) 
  {
    // ignore 
  }

  public void columnMoved(TableColumnModelEvent e) 
  {
    // ignore
  }

  public void columnRemoved(TableColumnModelEvent e) 
  {
    // ignore
  }

  public void columnSelectionChanged(ListSelectionEvent e) 
  {
    // ignore
  }

  public void test(TestHarness harness)
  {
    testGeneral(harness);
    testChangeEventInitialization(harness);
  }
  
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void testGeneral(TestHarness harness)      
  {
    harness.checkPoint("DefaultTableColumnModel()");
    DefaultTableColumnModel m1 = new DefaultTableColumnModel();
    harness.check(m1.getColumnCount(), 0);
    harness.check(m1.getColumnMargin(), 1);
    harness.check(m1.getColumnSelectionAllowed(), false);
    harness.check(m1.getSelectedColumnCount(), 0);
  }
  
  /**
   * This test confirms that the change event is created lazily.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  private void testChangeEventInitialization(TestHarness harness) 
  {
    harness.checkPoint("testChangeEventInitialization()");
    MyDefaultTableColumnModel m = new MyDefaultTableColumnModel();
    harness.check(m.getChangeEventField(), null);
    
    // no listeners, no need to create shared ChangeEvent
    m.setColumnMargin(99);
    harness.check(m.getChangeEventField(), null);
    
    // add a listener, and the event is generated
    m.addColumnModelListener(this);
    m.setColumnMargin(100);
    harness.check(m.getChangeEventField() != null);
  }

}
