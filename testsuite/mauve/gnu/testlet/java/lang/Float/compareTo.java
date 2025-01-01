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

package gnu.testlet.java.lang.Float;

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
    harness.checkPoint("compareTo(Float)");
    Float f1 = new Float(Float.NEGATIVE_INFINITY);
    Float f2 = new Float(-1.0);
    Float f3 = new Float(0.0);
    Float f4 = new Float(1.0);
    Float f5 = new Float(Float.POSITIVE_INFINITY);
    
    harness.check(f1.compareTo(f1) == 0);
    harness.check(f1.compareTo(f2) < 0);
    harness.check(f1.compareTo(f3) < 0);
    harness.check(f1.compareTo(f4) < 0);
    harness.check(f1.compareTo(f5) < 0);

    harness.check(f2.compareTo(f1) > 0);
    harness.check(f2.compareTo(f2) == 0);
    harness.check(f2.compareTo(f3) < 0);
    harness.check(f2.compareTo(f4) < 0);
    harness.check(f2.compareTo(f5) < 0);

    harness.check(f3.compareTo(f1) > 0);
    harness.check(f3.compareTo(f2) > 0);
    harness.check(f3.compareTo(f3) == 0);
    harness.check(f3.compareTo(f4) < 0);
    harness.check(f3.compareTo(f5) < 0);
    
    harness.check(f4.compareTo(f1) > 0);
    harness.check(f4.compareTo(f2) > 0);
    harness.check(f4.compareTo(f3) > 0);
    harness.check(f4.compareTo(f4) == 0);
    harness.check(f4.compareTo(f5) < 0);

    harness.check(f5.compareTo(f1) > 0);
    harness.check(f5.compareTo(f2) > 0);
    harness.check(f5.compareTo(f3) > 0);
    harness.check(f5.compareTo(f4) > 0);
    harness.check(f5.compareTo(f5) == 0);
    
    boolean pass = false;
    try 
    {
      f1.compareTo((Float) null);
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
    Float f1 = new Float(Float.NEGATIVE_INFINITY);
    boolean pass = false;
    try 
    {
      ((Comparable)f1).compareTo((Object) null);
    }
    catch (NullPointerException e) 
    {
      pass = true;  
    }
    harness.check(pass);
    
    pass = false;
    try 
    {
      ((Comparable)f1).compareTo("Not a Float!");
    }
    catch (ClassCastException e) 
    {
      pass = true;  
    }
    harness.check(pass);
  }

}
