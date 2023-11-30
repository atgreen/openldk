// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

// This file is part of Mauve.

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
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.lang.Double;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Some tests for the compareTo() method in the {@link Float} class.
 */
public class compareTo implements Testlet 
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
    harness.checkPoint("compareTo(Double)");
    Double d1 = new Double(Double.NEGATIVE_INFINITY);
    Double d2 = new Double(-1.0);
    Double d3 = new Double(0.0);
    Double d4 = new Double(1.0);
    Double d5 = new Double(Double.POSITIVE_INFINITY);
 
    harness.check(d1.compareTo(d1) == 0);
    harness.check(d1.compareTo(d2) < 0);
    harness.check(d1.compareTo(d3) < 0);
    harness.check(d1.compareTo(d4) < 0);
    harness.check(d1.compareTo(d5) < 0);

    harness.check(d2.compareTo(d1) > 0);
    harness.check(d2.compareTo(d2) == 0);
    harness.check(d2.compareTo(d3) < 0);
    harness.check(d2.compareTo(d4) < 0);
    harness.check(d2.compareTo(d5) < 0);

    harness.check(d3.compareTo(d1) > 0);
    harness.check(d3.compareTo(d2) > 0);
    harness.check(d3.compareTo(d3) == 0);
    harness.check(d3.compareTo(d4) < 0);
    harness.check(d3.compareTo(d5) < 0);
 
    harness.check(d4.compareTo(d1) > 0);
    harness.check(d4.compareTo(d2) > 0);
    harness.check(d4.compareTo(d3) > 0);
    harness.check(d4.compareTo(d4) == 0);
    harness.check(d4.compareTo(d5) < 0);

    harness.check(d5.compareTo(d1) > 0); 
    harness.check(d5.compareTo(d2) > 0);
    harness.check(d5.compareTo(d3) > 0);
    harness.check(d5.compareTo(d4) > 0);
    harness.check(d5.compareTo(d5) == 0);
 
    boolean pass = false;
    try 
    {
      ((Comparable)d1).compareTo((Float) null);
    }
    catch (NullPointerException e) 
    {
      pass = true;  
    }
    harness.check(pass);
  }

  public void test2(TestHarness harness)       
  {
    harness.checkPoint("compareTo(Object)");
    Double d1 = new Double(Double.NEGATIVE_INFINITY);
    
    boolean pass = false;
    try 
    {
      ((Comparable)d1).compareTo((Object) null);
    }
    catch (NullPointerException e) 
    {
      pass = true;  
    }
    harness.check(pass);
    
    pass = false;
    try 
    {
      ((Comparable)d1).compareTo("Not a Double!");
    }
    catch (ClassCastException e) 
    {
      pass = true;  
    }
    harness.check(pass);
    
  }
}
