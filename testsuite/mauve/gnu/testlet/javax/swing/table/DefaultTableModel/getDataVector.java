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

import java.util.Vector;

import javax.swing.table.DefaultTableModel;

/**
 * Some tests for the getDataVector() method in the {@link DefaultTableModel} class.
 */
public class getDataVector implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    DefaultTableModel m1 = new DefaultTableModel();
    Vector v1 = m1.getDataVector();
    harness.check(v1.isEmpty());

    Object[][] data2 = new Object[2][3];
    data2[0][0] = "V1";
    data2[0][1] = "V2";
    data2[0][2] = "V3";
    data2[1][0] = "V4";
    data2[1][1] = "V5";
    data2[1][2] = "V6";
    Object[] columns2 = new Object[] { "C1", "C2", "C3" };
    DefaultTableModel m2 = new DefaultTableModel(data2, columns2);
    Vector v2 = m2.getDataVector();
    harness.check(v2.size(), 2);
    Vector v2a = (Vector) v2.get(0);
    Vector v2b = (Vector) v2.get(1);
    harness.check(v2a.get(0), "V1");
    harness.check(v2a.get(1), "V2");
    harness.check(v2a.get(2), "V3");
    harness.check(v2b.get(0), "V4");
    harness.check(v2b.get(1), "V5");
    harness.check(v2b.get(2), "V6");
}

}
