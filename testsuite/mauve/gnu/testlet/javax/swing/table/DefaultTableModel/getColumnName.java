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

import javax.swing.table.DefaultTableModel;

/**
 * Some tests for the getColumnName() method in the {@link DefaultTableModel} 
 * class.
 */
public class getColumnName implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    DefaultTableModel m1 = new DefaultTableModel(2, 53);
    harness.check(m1.getColumnName(0), "A");
    harness.check(m1.getColumnName(26), "AA");
    harness.check(m1.getColumnName(52), "BA");
    
    // negative column index - spec doesn't say what exception is thrown, 
    // so we'll just check for a runtime exception
    boolean pass = false;
    try
    {
      /*String n =*/ m1.getColumnName(-1);
    }
    catch (RuntimeException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // column index too large - this should fetch a default column name
    harness.check(m1.getColumnName(99), "CV");
  }

}
