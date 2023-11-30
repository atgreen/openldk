// Tags: JDK1.1

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

package gnu.testlet.java.awt.ColorClass;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;

/**
 * Some checks for the decode() method in the {@link Color} class.  
 */
public class decode implements Testlet 
{

  /**
   * Parse the given string, create Color object using this string
   * and check if all three color component are set correcty.
   */
  private void checkColorDecode(TestHarness harness, String str, int red, int green, int blue)
  {
    Color c = Color.decode(str);
    harness.check(c.getRed(), red);
    harness.check(c.getGreen(), green);
    harness.check(c.getBlue(), blue);
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    // hexadecimal numbers interpretation
    checkColorDecode(harness, "0x0", 0, 0, 0);
    checkColorDecode(harness, "0X0", 0, 0, 0);
    checkColorDecode(harness, "#0", 0, 0, 0);
    checkColorDecode(harness, "0x010203", 1, 2, 3);
    checkColorDecode(harness, "0X010203", 1, 2, 3);
    checkColorDecode(harness, "#010203", 1, 2, 3);
    checkColorDecode(harness, "0xffffff", 255, 255, 255);
    checkColorDecode(harness, "0Xffffff", 255, 255, 255);
    checkColorDecode(harness, "#ffffff", 255, 255, 255);

    // negative hexadecimal numbers
    checkColorDecode(harness, "-0x0", 0, 0, 0);
    checkColorDecode(harness, "-0X0", 0, 0, 0);
    checkColorDecode(harness, "-#0", 0, 0, 0);
    checkColorDecode(harness, "-0x1", 255, 255, 255);
    checkColorDecode(harness, "-0X1", 255, 255, 255);
    checkColorDecode(harness, "-#1", 255, 255, 255);
    checkColorDecode(harness, "-0xffffff", 0, 0, 1);
    checkColorDecode(harness, "-0Xffffff", 0, 0, 1);
    checkColorDecode(harness, "-#ffffff", 0, 0, 1);
  
    // decimal numbers interpretation
    checkColorDecode(harness, "0", 0, 0, 0);
    checkColorDecode(harness, "255", 0, 0, 255);
    checkColorDecode(harness, "256", 0, 1, 0);
    checkColorDecode(harness, "65535", 0, 255, 255);
    checkColorDecode(harness, "65536", 1, 0, 0);
    checkColorDecode(harness, "16777215", 255, 255, 255);
    checkColorDecode(harness, "-16777215", 0, 0, 1);
    checkColorDecode(harness, "-1", 255, 255, 255);

    // octal numbers interpretation
    checkColorDecode(harness, "00", 0, 0, 0);
    checkColorDecode(harness, "0777", 0, 1, 255);
    checkColorDecode(harness, "077777777", 255, 255, 255);
    checkColorDecode(harness, "-01", 255, 255, 255);

    // try a null argument - see bug parade 6211249
    boolean pass = false;
    try
    {
      Color.decode(null);   
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
      Color.decode("XYZ");
    }
    catch (NumberFormatException e) 
    {
      pass = true;   
    }
    harness.check(pass);

    // try a bad octal value
    pass = false;
    try
    {
      Color.decode("0778");
    }
    catch (NumberFormatException e) 
    {
      pass = true;   
    }
    harness.check(pass);
  
  }

}
