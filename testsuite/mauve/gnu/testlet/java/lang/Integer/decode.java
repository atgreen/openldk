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
// Boston, MA 02111-1307, USA.  */

// Tags: JDK1.0

package gnu.testlet.java.lang.Integer;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Some checks for the decode() method in the {@link Integer} class.  
 */
public class decode implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    // decimal values
    harness.check(Integer.decode("0").equals(new Integer(0)));
    harness.check(Integer.decode("-1").equals(new Integer(-1)));
    harness.check(Integer.decode("123").equals(new Integer(123)));
    harness.check(Integer.decode("1234567").equals(new Integer(1234567)));
    harness.check(Integer.decode("2147483647").equals(new Integer(2147483647)));
    harness.check(Integer.decode("-2147483648").equals(new Integer(-2147483648)));
    
    // hexadecimal values
    harness.check(Integer.decode("0x00").equals(new Integer(0)));
    harness.check(Integer.decode("-0x01").equals(new Integer(-1)));
    harness.check(Integer.decode("0xFF").equals(new Integer(255)));
    harness.check(Integer.decode("0XFF").equals(new Integer(255)));
    harness.check(Integer.decode("0xff").equals(new Integer(255)));
    harness.check(Integer.decode("0XfF").equals(new Integer(255)));
    harness.check(Integer.decode("#ff").equals(new Integer(255)));
    
    // octal values
    harness.check(Integer.decode("00").equals(new Integer(0)));
    harness.check(Integer.decode("-070").equals(new Integer(-56)));
    harness.check(Integer.decode("072").equals(new Integer(58)));
    
    // try a null argument
    boolean pass = false;
    try
    {
      Integer.decode(null);   
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try a non-numeric string
    pass = false;
    try
    {
      Integer.decode("XYZ");
    }
    catch (NumberFormatException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try some bad formatting
    pass = false;
    try
    {
      Integer.decode("078");   
    }
    catch (NumberFormatException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      Integer.decode("1.0");   
    }
    catch (NumberFormatException e)
    {
      pass = true;   
    }
    harness.check(pass);

    pass = false;
    try
    {
      Integer.decode("");   
    }
    catch (NumberFormatException e)
    {
      pass = true;   
    }
    harness.check(pass);

  }

}
