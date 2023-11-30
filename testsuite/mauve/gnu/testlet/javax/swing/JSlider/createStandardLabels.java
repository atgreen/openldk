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
 
package gnu.testlet.javax.swing.JSlider;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Hashtable;

import javax.swing.JSlider;

/**
 * Some checks for the createStandardLabels() method in the {@link JSlider} 
 * class.
 */
public class createStandardLabels implements Testlet {

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
    harness.checkPoint("createStandardLabels(int)");
    JSlider slider = new JSlider();
    Hashtable table = slider.createStandardLabels(10);
    harness.check(table.containsKey(new Integer(0)));
    harness.check(table.containsKey(new Integer(10)));
    harness.check(table.containsKey(new Integer(20)));
    harness.check(table.containsKey(new Integer(30)));
    harness.check(table.containsKey(new Integer(40)));
    harness.check(table.containsKey(new Integer(50)));
    harness.check(table.containsKey(new Integer(60)));
    harness.check(table.containsKey(new Integer(70)));
    harness.check(table.containsKey(new Integer(80)));
    harness.check(table.containsKey(new Integer(90)));
    harness.check(table.containsKey(new Integer(100)));
    
    // try 0
    boolean pass = false;
    try
    {
      /*Hashtable t1 =*/ slider.createStandardLabels(0);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try negative
    pass = false;
    try
    {
      /*Hashtable t1 =*/ slider.createStandardLabels(-1);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  public void test2(TestHarness harness)      
  {
    harness.checkPoint("createStandardLabels(int, int)");
    JSlider slider = new JSlider();
    Hashtable table = slider.createStandardLabels(10, 50);
    harness.check(table.containsKey(new Integer(50)));
    harness.check(table.containsKey(new Integer(60)));
    harness.check(table.containsKey(new Integer(70)));
    harness.check(table.containsKey(new Integer(80)));
    harness.check(table.containsKey(new Integer(90)));
    harness.check(table.containsKey(new Integer(100)));
    
    table = slider.createStandardLabels(10, 51);
    harness.check(table.containsKey(new Integer(51)));
    harness.check(table.containsKey(new Integer(61)));
    harness.check(table.containsKey(new Integer(71)));
    harness.check(table.containsKey(new Integer(81)));
    harness.check(table.containsKey(new Integer(91)));

    table = slider.createStandardLabels(10, 100);
    harness.check(table.containsKey(new Integer(100)));

    // try 0 for increment
    boolean pass = false;
    try
    {
      /*Hashtable t1 =*/ slider.createStandardLabels(0, 50);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try negative increment
    pass = false;
    try
    {
      /*Hashtable t1 =*/ slider.createStandardLabels(-1, 50);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try initial value < minimum
    pass = false;
    try
    {
      /*Hashtable t1 =*/ slider.createStandardLabels(10, -50);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try initial value > maximum
    pass = false;
    try
    {
      /*Hashtable t1 =*/ slider.createStandardLabels(10, 110);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
