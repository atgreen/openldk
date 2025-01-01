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

package gnu.testlet.java.awt.TextField;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.TextField;

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
    
    TextField field = new TextField();
    harness.check(field.getSize(), new Dimension());
    harness.check(field.getMinimumSize(), new Dimension());
    
    // Check that if mininum size has not been set, then a 
    // Dimension with current number of rows and columns
    // is returned.
    harness.check(field.isMinimumSizeSet(), false);
    field.setSize(new Dimension(4, 8));
    harness.check(field.getMinimumSize(), new Dimension(4, 8));
        
    // Check that if minimum size has been set,
    // then those values are returned.
    Dimension minSize = new Dimension(5, 16);
    field.setMinimumSize(minSize);
    harness.check(field.isMinimumSizeSet());
    harness.check(field.getMinimumSize() != minSize);
    harness.check(field.getMinimumSize(), minSize); 
  }
  
  public void test2(TestHarness harness)
  {
    // Test getMinimumSize(int, int) method.
    
    TextField field = new TextField();
    
    // Show that if the values passed are <=, ==, >= or negative, 
    // then no exceptions are thrown.
    harness.check(field.getSize(), new Dimension());
    harness.check(field.getMinimumSize(7), new Dimension());
    harness.check(field.getMinimumSize(0), new Dimension());
    harness.check(field.getMinimumSize(-7), new Dimension());
        
    // Check that if minimum size not been set, then a 
    // Dimension with size (width, height) is returned.
    harness.check(field.isMinimumSizeSet(), false);
    field.setSize(new Dimension(3, 4));
    harness.check(field.getMinimumSize(2), new Dimension(3, 4));
    
    // Check that if minimum size has been set, then a
    // those values are returned.
    field.setSize(new Dimension(-3, 5));
    harness.check(field.getSize(), new Dimension(-3, 5));
    field.setMinimumSize(new Dimension(1, 9));
    harness.check(field.isMinimumSizeSet());
    harness.check(field.getMinimumSize(1), new Dimension(1, 9));
  }
  
  public void test3(TestHarness harness)
  {
    // This test will show that if peer == null, then the getWidth()
    // and getHeight() values are used (and not getRows() and getColumns()
    // or 0 and 0).
    
    TextField field = new TextField();
    field.setBounds(1, 2, 3, 4);
    harness.check(field.getX(), 1);
    harness.check(field.getY(), 2);
    harness.check(field.getWidth(), 3);
    harness.check(field.getHeight(), 4);
    harness.check(field.getColumns(), 0);
    
    harness.check(field.getPeer() == null);
    harness.check(field.getMinimumSize(), 
                  new Dimension(field.getWidth(), field.getHeight()));
  }
  
  public void test4(TestHarness harness)
  {
    // This test shows that the text field's font does not
    // affect the value of its minimum size.
    
    TextField field = new TextField();
    
    field.setFont(new Font("Dialog-PLAIN-12", Font.ITALIC, 58));
    harness.check(field.getMinimumSize(), new Dimension());
    field.setFont(new Font("TimesRoman", Font.BOLD, 2));
    harness.check(field.getMinimumSize(), new Dimension());
  }
  
  public void test5(TestHarness harness)
  {
    // This test shows that the text field's preferred size
    // does not affect the value of its minimum size.
    
    TextField field = new TextField();
    
    field.setPreferredSize(new Dimension(7, 3));
    harness.check(field.isPreferredSizeSet());
    harness.check(field.getMinimumSize(), new Dimension());
    field.setPreferredSize(new Dimension());
    harness.check(field.getMinimumSize(), new Dimension());
    field.setPreferredSize(new Dimension(-3, -6));
    harness.check(field.getMinimumSize(), new Dimension());
  }

}
