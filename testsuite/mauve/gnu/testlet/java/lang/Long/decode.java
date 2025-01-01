// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>
// Updated to use for Long class: Pavel Tisnovsky <ptisnovs@redhat.com>

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

package gnu.testlet.java.lang.Long;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Some checks for the decode() method in the {@link Long} class.  
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
    harness.check(Long.decode("0").equals(new Long(0)));
    harness.check(Long.decode("-1").equals(new Long(-1)));
    harness.check(Long.decode("123").equals(new Long(123)));
    harness.check(Long.decode("1234567").equals(new Long(1234567)));
    harness.check(Long.decode("1234567890000").equals(new Long(1234567890000L)));
    harness.check(Long.decode("2147483647").equals(new Long(2147483647)));
    harness.check(Long.decode("-2147483648").equals(new Long(-2147483648)));
    
    // hexadecimal values
    harness.check(Long.decode("0x00").equals(new Long(0)));
    harness.check(Long.decode("-0x01").equals(new Long(-1)));
    harness.check(Long.decode("0xFF").equals(new Long(255)));
    harness.check(Long.decode("0XFF").equals(new Long(255)));
    harness.check(Long.decode("0xff").equals(new Long(255)));
    harness.check(Long.decode("0XfF").equals(new Long(255)));
    harness.check(Long.decode("#ff").equals(new Long(255)));
    harness.check(Long.decode("0Xffff").equals(new Long(65535)));
    
    // octal values
    harness.check(Long.decode("00").equals(new Long(0)));
    harness.check(Long.decode("-070").equals(new Long(-56)));
    harness.check(Long.decode("072").equals(new Long(58)));
    
    // try a null argument
    boolean pass = false;
    try
    {
      Long.decode(null);   
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
      Long.decode("XYZ");
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
      Long.decode("078");   
    }
    catch (NumberFormatException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      Long.decode("1.0");   
    }
    catch (NumberFormatException e)
    {
      pass = true;   
    }
    harness.check(pass);

    pass = false;
    try
    {
      Long.decode("");   
    }
    catch (NumberFormatException e)
    {
      pass = true;   
    }
    harness.check(pass);

  }

}
