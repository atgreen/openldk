// Tags: JDK1.2

// Copyright (C) 2005, 2006, David Gilbert <david.gilbert@object-refinery.com>

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

import javax.swing.DefaultListSelectionModel;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableColumnModel;

/**
 * Some tests for the setSelectionModel() method in the 
 * {@link DefaultTableColumnModel} class.
 */
public class setSelectionModel implements Testlet
{

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    DefaultTableColumnModel m1 = new DefaultTableColumnModel();
    DefaultListSelectionModel lsm = new DefaultListSelectionModel();
    m1.setSelectionModel(lsm);
    harness.check(m1.getSelectionModel(), lsm);
    ListSelectionListener[] listeners = lsm.getListSelectionListeners();
    harness.check(listeners[0], m1);
    
    DefaultListSelectionModel lsm2 = new DefaultListSelectionModel();
    m1.setSelectionModel(lsm2);
    harness.check(m1.getSelectionModel(), lsm2);
    listeners = lsm.getListSelectionListeners();
    harness.check(listeners.length, 0);
    
    boolean pass = false;
    try
    {
      m1.setSelectionModel(null);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
  }

}
