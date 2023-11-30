/* constructors.java -- some checks for the constructors in the 
                        SpinnerNumberModel class.
   Copyright (C) 2006  David Gilbert <david.gilbert@object-refinery.com>
   
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

// Tags: JDK1.4

package gnu.testlet.javax.swing.SpinnerNumberModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.SpinnerNumberModel;

/**
 * Some tests for the constructors in the {@link SpinnerNumberModel} class.
 */
public class constructors implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
  }

  private void testConstructor1(TestHarness harness) 
  {
    harness.checkPoint("()");
    SpinnerNumberModel m = new SpinnerNumberModel();
    harness.check(m.getValue(), new Integer(0));
    harness.check(m.getMinimum(), null);
    harness.check(m.getMaximum(), null);
    harness.check(m.getStepSize(), new Integer(1));
  }

  private void testConstructor2(TestHarness harness)  
  {
    harness.checkPoint("(double, double, double, double)");
    SpinnerNumberModel m = new SpinnerNumberModel(2.0, 1.0, 3.0, 0.5);
    harness.check(m.getValue(), new Double(2.0));
    harness.check(m.getMinimum(), new Double(1.0));
    harness.check(m.getMaximum(), new Double(3.0));
    harness.check(m.getStepSize(), new Double(0.5));
    
    // value equal to minimum 
    m = new SpinnerNumberModel(1.0, 1.0, 3.0, 0.5);
    harness.check(m.getValue(), new Double(1.0));
    harness.check(m.getMinimum(), new Double(1.0));
    harness.check(m.getMaximum(), new Double(3.0));
    harness.check(m.getStepSize(), new Double(0.5));

    // value below minimum
    boolean pass = false;
    try
    {
      m = new SpinnerNumberModel(0.9, 1.0, 3.0, 0.5);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // value equal to maximum
    m = new SpinnerNumberModel(3.0, 1.0, 3.0, 0.5);
    harness.check(m.getValue(), new Double(3.0));
    harness.check(m.getMinimum(), new Double(1.0));
    harness.check(m.getMaximum(), new Double(3.0));
    harness.check(m.getStepSize(), new Double(0.5));
    
    // value above maximum
    pass = false;
    try
    {
      m = new SpinnerNumberModel(3.1, 1.0, 3.0, 0.5);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // step size 0
    m = new SpinnerNumberModel(2.0, 1.0, 3.0, 0.0);
    harness.check(m.getValue(), new Double(2.0));
    harness.check(m.getMinimum(), new Double(1.0));
    harness.check(m.getMaximum(), new Double(3.0));
    harness.check(m.getStepSize(), new Double(0.0));
    
    // step size negative
    m = new SpinnerNumberModel(2.0, 1.0, 3.0, -0.5);
    harness.check(m.getValue(), new Double(2.0));
    harness.check(m.getMinimum(), new Double(1.0));
    harness.check(m.getMaximum(), new Double(3.0));
    harness.check(m.getStepSize(), new Double(-0.5));
    
  }
  
  private void testConstructor3(TestHarness harness) 
  {
    harness.checkPoint("(int, int, int, int)"); 
    SpinnerNumberModel m = new SpinnerNumberModel(20, 10, 30, 5);
    harness.check(m.getValue(), new Integer(20));
    harness.check(m.getMinimum(), new Integer(10));
    harness.check(m.getMaximum(), new Integer(30));
    harness.check(m.getStepSize(), new Integer(5));

    // value equal to minimum 
    m = new SpinnerNumberModel(10, 10, 30, 5);
    harness.check(m.getValue(), new Integer(10));
    harness.check(m.getMinimum(), new Integer(10));
    harness.check(m.getMaximum(), new Integer(30));
    harness.check(m.getStepSize(), new Integer(5));

    // value below minimum
    boolean pass = false;
    try
    {
      m = new SpinnerNumberModel(9, 10, 30, 5);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // value equal to maximum
    m = new SpinnerNumberModel(30, 10, 30, 5);
    harness.check(m.getValue(), new Integer(30));
    harness.check(m.getMinimum(), new Integer(10));
    harness.check(m.getMaximum(), new Integer(30));
    harness.check(m.getStepSize(), new Integer(5));
    
    // value above maximum
    pass = false;
    try
    {
      m = new SpinnerNumberModel(31, 10, 30, 5);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // step size 0
    m = new SpinnerNumberModel(20, 10, 30, 0);
    harness.check(m.getValue(), new Integer(20));
    harness.check(m.getMinimum(), new Integer(10));
    harness.check(m.getMaximum(), new Integer(30));
    harness.check(m.getStepSize(), new Integer(0));
    
    // step size negative
    m = new SpinnerNumberModel(20, 10, 30, -5);
    harness.check(m.getValue(), new Integer(20));
    harness.check(m.getMinimum(), new Integer(10));
    harness.check(m.getMaximum(), new Integer(30));
    harness.check(m.getStepSize(), new Integer(-5));

  }
  
  private void testConstructor4(TestHarness harness)  
  {
    harness.checkPoint("Number, Comparable, Comparable, Number");
    SpinnerNumberModel m = new SpinnerNumberModel(new Long(20), new Long(10), 
            new Long(30), new Long(5));
    harness.check(m.getValue(), new Long(20));
    harness.check(m.getMinimum(), new Long(10));
    harness.check(m.getMaximum(), new Long(30));
    harness.check(m.getStepSize(), new Long(5));
    
    // value equal to minimum 
    m = new SpinnerNumberModel(new Double(1.0), new Double(1.0), 
            new Double(3.0), new Double(0.5));
    harness.check(m.getValue(), new Double(1.0));
    harness.check(m.getMinimum(), new Double(1.0));
    harness.check(m.getMaximum(), new Double(3.0));
    harness.check(m.getStepSize(), new Double(0.5));

    // value below minimum
    boolean pass = false;
    try
    {
      m = new SpinnerNumberModel(new Double(0.9), new Double(1.0), 
              new Double(3.0), new Double(0.5));
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // value equal to maximum
    m = new SpinnerNumberModel(new Double(3.0), new Double(1.0), 
            new Double(3.0), new Double(0.5));
    harness.check(m.getValue(), new Double(3.0));
    harness.check(m.getMinimum(), new Double(1.0));
    harness.check(m.getMaximum(), new Double(3.0));
    harness.check(m.getStepSize(), new Double(0.5));
    
    // value above maximum
    pass = false;
    try
    {
      m = new SpinnerNumberModel(new Double(3.1), new Double(1.0), 
              new Double(3.0), new Double(0.5));
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // step size 0
    m = new SpinnerNumberModel(new Double(2.0), new Double(1.0), 
            new Double(3.0), new Double(0.0));
    harness.check(m.getValue(), new Double(2.0));
    harness.check(m.getMinimum(), new Double(1.0));
    harness.check(m.getMaximum(), new Double(3.0));
    harness.check(m.getStepSize(), new Double(0.0));
    
    // step size negative
    m = new SpinnerNumberModel(new Double(2.0), new Double(1.0), 
            new Double(3.0), new Double(-0.5));
    harness.check(m.getValue(), new Double(2.0));
    harness.check(m.getMinimum(), new Double(1.0));
    harness.check(m.getMaximum(), new Double(3.0));
    harness.check(m.getStepSize(), new Double(-0.5));
    
    // check null value
    pass = false;
    try
    {
      /*SpinnerNumberModel m =*/ new SpinnerNumberModel(null, new Long(10), 
            new Long(30), new Long(5));
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);

    // check null minimum
    
    // check null maximum
    
    // check null step size
    pass = false;
    try
    {
      /*SpinnerNumberModel m =*/ new SpinnerNumberModel(new Long(20), 
              new Long(10), new Long(30), null);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
  
  }

}
