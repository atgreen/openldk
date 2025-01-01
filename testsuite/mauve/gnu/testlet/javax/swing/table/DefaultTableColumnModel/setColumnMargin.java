// Tags: JDK1.2
// Uses: MyListener

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
import javax.swing.table.DefaultTableColumnModel;

/**
 * Some tests for the setColumnMargin(int) method in the 
 * {@link DefaultTableColumnModel} class.
 */
public class setColumnMargin implements Testlet
{

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    DefaultTableColumnModel m1 = new DefaultTableColumnModel();
    MyListener listener = new MyListener();
    m1.addColumnModelListener(listener);
    m1.setColumnMargin(123);
    harness.check(m1.getColumnMargin(), 123);
    ChangeEvent e = listener.getChangeEvent();
    harness.check(e != null);
    harness.check(e.getSource(), m1);
    
    // try a negative value
    m1.setColumnMargin(-99);
    harness.check(m1.getColumnMargin(), -99);
  }

}
