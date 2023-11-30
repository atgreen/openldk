// Tags: JDK1.2
// Uses: MyDefaultTableModel

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
 * Some tests for the convertToVector() methods in the {@link DefaultTableModel} class.
 */
public class convertToVector implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    test1(harness);
    test2(harness);
  }
  
  public void test1(TestHarness harness) 
  {
    Object[] data = new Object[] {"X", "Y", null, "Z"};
    Vector v1 = MyDefaultTableModel.convertToVector(data);
    harness.check(v1.get(0), "X");
    harness.check(v1.get(1), "Y");
    harness.check(v1.get(2), null);
    harness.check(v1.get(3), "Z");
    
    Object[] data2 = new Object[] {};
    Vector v2 = MyDefaultTableModel.convertToVector(data2);
    harness.check(v2.isEmpty());
    
    Vector v3 = MyDefaultTableModel.convertToVector((Object[]) null);
    harness.check(v3 == null);
  }

  public void test2(TestHarness harness) 
  {
    Object[] data1 = new Object[] {"X", "Y", null, "Z"};
    Object[] data2 = new Object[] {"A", "B" };
    Object[][] data = new Object[2][];
    data[0] = data1;
    data[1] = data2;
    Vector vector = MyDefaultTableModel.convertToVector(data);
    Vector v1 = (Vector) vector.get(0);
    harness.check(v1.get(0), "X");
    harness.check(v1.get(1), "Y");
    harness.check(v1.get(2), null);
    harness.check(v1.get(3), "Z");
    harness.check(v1.size(), 4);
    
    Vector v2 = (Vector) vector.get(1);
    harness.check(v2.get(0), "A");
    harness.check(v2.get(1), "B");
    harness.check(v2.size(), 2);
    
    Object[][] data3 = new Object[][] {};
    Vector v3 = MyDefaultTableModel.convertToVector(data3);
    harness.check(v3.isEmpty());
    
    Vector v4 = MyDefaultTableModel.convertToVector((Object[][]) null);
    harness.check(v4 == null);
  }

}
