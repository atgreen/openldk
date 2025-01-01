/* getMinimumSize.java 
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

// Tags: JDK1.1

package gnu.testlet.java.awt.TextArea;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.TextArea;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class getMinimumSize implements Testlet
{

  public void test(TestHarness harness)
  {
    test1(harness);    
    test2(harness);   
    test3(harness);
    test4(harness);
    test5(harness);
  }
  
  public void test1(TestHarness harness)
  {
    // Test getMinimumSize() method.
    
    TextArea area = new TextArea();
    harness.check(area.getSize(), new Dimension());
    harness.check(area.getMinimumSize(), new Dimension());
    
    // Check that if mininum size has not been set, then a 
    // Dimension with current number of rows and columns
    // is returned.
    harness.check(area.isMinimumSizeSet(), false);
    area.setSize(new Dimension(4, 8));
    harness.check(area.getMinimumSize(), new Dimension(4, 8));
        
    // Check that if minimum size has been set,
    // then those values are returned.
    Dimension minSize = new Dimension(5, 16);
    area.setMinimumSize(minSize);
    harness.check(area.isMinimumSizeSet());
    harness.check(area.getMinimumSize() != minSize);
    harness.check(area.getMinimumSize(), minSize); 
  }
  
  public void test2(TestHarness harness)
  {
    // Test getMinimumSize(int, int) method.
    
    TextArea area = new TextArea();
    
    // Show that if the values passed are <=, ==, >= or negative, 
    // then no exceptions are thrown.
    harness.check(area.getSize(), new Dimension());
    harness.check(area.getMinimumSize(6, 7), new Dimension());
    harness.check(area.getMinimumSize(0, 0), new Dimension());
    harness.check(area.getMinimumSize(-4, -7), new Dimension());
        
    // Check that if minimum size not been set, then a 
    // Dimension with size (width, height) is returned.
    harness.check(area.isMinimumSizeSet(), false);
    area.setSize(new Dimension(3, 4));
    harness.check(area.getMinimumSize(3, 2), new Dimension(3, 4));
    
    // Check that if minimum size has been set, then a
    // those values are returned.
    area.setSize(new Dimension(-3, 5));
    harness.check(area.getSize(), new Dimension(-3, 5));
    area.setMinimumSize(new Dimension(1, 9));
    harness.check(area.isMinimumSizeSet());
    harness.check(area.getMinimumSize(-1, 1), new Dimension(1, 9));
  }
  
  public void test3(TestHarness harness)
  {
    // This test will show that if peer == null, then the getWidth()
    // and getHeight() values are used (and not getRows() and getColumns()
    // or 0 and 0).
    
    TextArea area = new TextArea();
    area.setBounds(1, 2, 3, 4);
    harness.check(area.getX(), 1);
    harness.check(area.getY(), 2);
    harness.check(area.getWidth(), 3);
    harness.check(area.getHeight(), 4);
    harness.check(area.getRows(), 0);
    harness.check(area.getColumns(), 0);
    
    harness.check(area.getPeer() == null);
    harness.check(area.getMinimumSize(), 
                  new Dimension(area.getWidth(), area.getHeight()));
  }
  
  public void test4(TestHarness harness)
  {
    // This test shows that the text area's font does not
    // affect the value of its minimum size.
    
    TextArea area = new TextArea();
    
    area.setFont(new Font("Dialog-PLAIN-12", Font.ITALIC, 58));
    harness.check(area.getMinimumSize(), new Dimension());
    area.setFont(new Font("TimesRoman", Font.BOLD, 2));
    harness.check(area.getMinimumSize(), new Dimension());
  }
  
  public void test5(TestHarness harness)
  {
    // This test shows that the text area's preferred size
    // does not affect the value of its minimum size.
    
    TextArea area = new TextArea();
    
    area.setPreferredSize(new Dimension(7, 3));
    harness.check(area.isPreferredSizeSet());
    harness.check(area.getMinimumSize(), new Dimension());
    area.setPreferredSize(new Dimension());
    harness.check(area.getMinimumSize(), new Dimension());
    area.setPreferredSize(new Dimension(-3, -6));
    harness.check(area.getMinimumSize(), new Dimension());
  }
 
}
