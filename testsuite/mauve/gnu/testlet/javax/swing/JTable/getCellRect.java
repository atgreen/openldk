/* getCellRect.java 
   Copyright (C) 2006 Red Hat
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.JTable;

import java.awt.Rectangle;

import javax.swing.JTable;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class getCellRect implements Testlet
{

  public void test(TestHarness harness)
  {   
    JTable table = new JTable(0, 0);
    Rectangle rectangle = table.getCellRect(0, 0, false);
    harness.check(rectangle.x, 0);
    harness.check(rectangle.y, 0);
    harness.check(rectangle.width, 0);
    harness.check(rectangle.height, 0);
    
    table = new JTable(2, 3);
    rectangle = table.getCellRect(1, 2, false);
    harness.check(rectangle.x, 150);
    harness.check(rectangle.y, 16);
    harness.check(rectangle.width, 74);
    harness.check(rectangle.height, 15);
    
    table = new JTable(2, 3);
    rectangle = table.getCellRect(0, 2, false);
    harness.check(rectangle.x, 150);
    harness.check(rectangle.y, 0);
    harness.check(rectangle.width, 74);
    harness.check(rectangle.height, 15);
    
    table = new JTable(2, 3);
    rectangle = table.getCellRect(1, 0, false);
    harness.check(rectangle.x, 0);
    harness.check(rectangle.y, 16);
    harness.check(rectangle.width, 74);
    harness.check(rectangle.height, 15);
       
    table = new JTable(0, 1);
    rectangle = table.getCellRect(0, 0, true);
    harness.check(rectangle.x, 0);
    harness.check(rectangle.y, 0);
    harness.check(rectangle.width, 75);
    harness.check(rectangle.height, 0);
    
    table = new JTable(2, 3);
    rectangle = table.getCellRect(1, 2, true);
    harness.check(rectangle.x, 150);
    harness.check(rectangle.y, 16);
    harness.check(rectangle.width, 75);
    harness.check(rectangle.height, 16);
    
    table = new JTable(2, 3);
    rectangle = table.getCellRect(0, 2, true);
    harness.check(rectangle.x, 150);
    harness.check(rectangle.y, 0);
    harness.check(rectangle.width, 75);
    harness.check(rectangle.height, 16);
    
    table = new JTable(2, 3);
    rectangle = table.getCellRect(1, 0, true);
    harness.check(rectangle.x, 0);
    harness.check(rectangle.y, 16);
    harness.check(rectangle.width, 75);
    harness.check(rectangle.height, 16);
    
    // try varying the row heights
    table = new JTable(2, 3);
    table.setRowHeight(0, 11);
    table.setRowHeight(1, 12);
    harness.check(table.getCellRect(0, 2, false), 
            new Rectangle(150, 0, 74, 10));
    harness.check(table.getCellRect(1, 2, false), 
            new Rectangle(150, 11, 74, 11));
    
    // try varying the column widths
    table = new JTable(2, 3);
    table.getColumnModel().getColumn(0).setWidth(11);
    table.getColumnModel().getColumn(1).setWidth(15);
    table.getColumnModel().getColumn(2).setWidth(17);
    harness.check(table.getCellRect(0, 0, false), new Rectangle(0, 0, 14, 15));
    harness.check(table.getCellRect(1, 1, false), 
            new Rectangle(15, 16, 14, 15));
    harness.check(table.getCellRect(0, 2, false), new Rectangle(30, 0, 16, 15));
    harness.check(table.getCellRect(1, 0, false), new Rectangle(0, 16, 14, 15));
    harness.check(table.getCellRect(0, 1, false), new Rectangle(15, 0, 14, 15));
    harness.check(table.getCellRect(1, 2, false), 
            new Rectangle(30, 16, 16, 15));
    
    // try negative row
    table = new JTable(2, 3);
    harness.check(table.getCellRect(-1, 0, true), new Rectangle(0, 0, 75, 0));
    
    // try row index too large
    table = new JTable(2, 3);
    harness.check(table.getCellRect(99, 0, true), new Rectangle(0, 0, 75, 0));
    
    // try negative column
    harness.check(table.getCellRect(0, -1, true), new Rectangle(0, 0, 0, 16));
    
    // try column index too large
    harness.check(table.getCellRect(0, 99, true), new Rectangle(0, 0, 0, 16));
  }

}
