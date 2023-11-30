//Tags: JDK1.2

//Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.RenderingHints;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.RenderingHints;

/**
 * Some checks for the constructors in the {@link RenderingHints}
 * class.
 */
public class constructors implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) {
    
    harness.checkPoint("RenderingHints(Map)");
    testConstructor1(harness);  
    
    harness.checkPoint("RenderingHints(Key, Object)");
    testConstructor2(harness);
  }
  
  public void testConstructor1(TestHarness harness) 
  {
    RenderingHints h = new RenderingHints(null);
    harness.check(h.isEmpty());
    harness.check(h.size(), 0);
  }

  public void testConstructor2(TestHarness harness) 
  {
    // it isn't clear from the spec whether a null key is acceptable, but to be
    // consistent with the put(key, value) method it should throw a 
    // NullPointerException
    boolean pass = false;
    try
    {
      RenderingHints h = new RenderingHints(
        null, RenderingHints.VALUE_ALPHA_INTERPOLATION_DEFAULT
      );
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // it isn't clear from the spec whether a null value is acceptable, but to be
    // consistent with the put(key, value) method it should throw a 
    // NullPointerException
    pass = false;
    try 
    {
      RenderingHints h = new RenderingHints(
        RenderingHints.KEY_ALPHA_INTERPOLATION, null
      );
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }
  
}
