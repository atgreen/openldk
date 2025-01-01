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
 * Some tests for the getValueAt() method in the {@link DefaultTableModel} class.
 */
public class getValueAt implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    DefaultTableModel m1 = new DefaultTableModel(2, 3);
    m1.setValueAt("V1", 0, 0);
    m1.setValueAt("V2", 0, 1);
    m1.setValueAt("V3", 0, 2);
    m1.setValueAt("V4", 1, 0);
    m1.setValueAt("V5", 1, 1);
    m1.setValueAt("V6", 1, 2);
    harness.check(m1.getValueAt(0, 0), "V1");
    harness.check(m1.getValueAt(0, 1), "V2");
    harness.check(m1.getValueAt(0, 2), "V3");
    harness.check(m1.getValueAt(1, 0), "V4");
    harness.check(m1.getValueAt(1, 1), "V5");
    harness.check(m1.getValueAt(1, 2), "V6");
  
    boolean pass = false;
    try 
    {
      m1.getValueAt(-1, 0);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);

    pass = false;
    try 
    {
      m1.getValueAt(99, 0);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);

    pass = false;
    try 
    {
      m1.getValueAt(0, -1);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);

    pass = false;
    try 
    {
      m1.getValueAt(0, 99);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);

  }

}
