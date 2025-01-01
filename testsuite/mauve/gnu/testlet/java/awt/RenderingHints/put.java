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
 * Some checks for the put() method in the {@link RenderingHints} class.
 */
public class put implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) {

    RenderingHints h1 = new RenderingHints(
      RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_OFF
    );
    h1.put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    harness.check(h1.size() == 2);
    harness.check(h1.containsKey(RenderingHints.KEY_ANTIALIASING));
    harness.check(h1.get(RenderingHints.KEY_ANTIALIASING).equals(RenderingHints.VALUE_ANTIALIAS_ON));

    // adding a value that is not compatible with the key generates an 
    // IllegalArgumentException...
    boolean pass = false;
    try
    {
      h1.put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY);
    }
    catch (IllegalArgumentException e)  
    {
      pass = true;
    }
    harness.check(pass);
    
    // adding a null value for a valid key should generate an 
    // IllegalArgumentException or a NullPointerException depending
    // on the order of the checks...
    pass = false;
    try
    {
      h1.put(RenderingHints.KEY_ANTIALIASING, null);
    }
    catch (IllegalArgumentException e)  
    {
      pass = true;
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // adding a key that is not a RenderingHints.Key generates a 
    // ClassCastException (or possibly a NullPointerException if 
    // the second argument is checked first)...
    pass = false;
    try
    {
      h1.put(new Integer(1), null);
    }
    catch (ClassCastException e)  
    {
      pass = true;
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // adding a null key generates a NullPointerException...
    pass = false;
    try
    {
      h1.put(null, null);
    }
    catch (NullPointerException e)  
    {
      pass = true;
    }
    harness.check(pass);

  }

}
