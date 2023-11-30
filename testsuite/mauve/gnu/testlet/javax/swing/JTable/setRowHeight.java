/* setRowHeight.java -- some checks for the setRowHeight() method in the
       JTable class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.5

package gnu.testlet.javax.swing.JTable;

import javax.swing.JTable;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class setRowHeight implements Testlet 
{
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
  }
  
  public void test1(TestHarness harness)
  {
    harness.checkPoint(("int"));
    JTable t = new JTable(4, 3);
    harness.check(t.getRowHeight(0), 16);
    t.setRowHeight(99);
    harness.check(t.getRowHeight(0), 99);
    
    // does this row height override individual row heights?
    t.setRowHeight(1, 13);
    harness.check(t.getRowHeight(1), 13);
    t.setRowHeight(14);
    harness.check(t.getRowHeight(1), 14);
    
    // try zero
    boolean pass = false;
    try
    {
      t.setRowHeight(0);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  public void test2(TestHarness harness)
  {
    harness.checkPoint("(int, int)");
    JTable t = new JTable(4, 3);
    harness.check(t.getRowHeight(0), 16);
    t.setRowHeight(99);
    harness.check(t.getRowHeight(0), 99);
    
  }
}
