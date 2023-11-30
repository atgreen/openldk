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

package gnu.testlet.java.awt.ColorClass;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;
import java.awt.color.ColorSpace;

/**
 * Some checks for the constructors in the {@link Color} class.  
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
    testConstructor5(harness);
    testConstructor6(harness);
    testConstructor7(harness);
  }

  private void testConstructor1(TestHarness harness) 
  {
    harness.checkPoint("(ColorSpace, float[], float)");
    Color c = new Color(ColorSpace.getInstance(ColorSpace.CS_sRGB), 
            new float[] { 0.2f, 0.4f, 0.6f}, 0.8f);
    harness.check(c.getRed(), 51);
    harness.check(c.getGreen(), 102);
    harness.check(c.getBlue(), 153);
    harness.check(c.getAlpha(), 204);

    // try null space
    boolean pass = false;
    try
    {
      c = new Color(null, new float[] { 0.2f, 0.4f, 0.6f}, 0.8f);
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);
  }
  
  private void testConstructor2(TestHarness harness) 
  {
    harness.checkPoint("(float, float, float)");
    Color c = new Color(0.2f, 0.4f, 0.6f);
    harness.check(c.getRed(), 51);
    harness.check(c.getGreen(), 102);
    harness.check(c.getBlue(), 153);
    harness.check(c.getAlpha(), 255);
 
    // negative red
    boolean pass = false;
    try
    {
      c = new Color(-0.2f, 0.4f, 0.6f);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
 
    // negative green
    pass = false;
    try
    {
      c = new Color(0.2f, -0.4f, 0.6f);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
     
    // negative blue
    pass = false;
    try
    {
      c = new Color(0.2f, 0.4f, -0.6f);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
 
    // red > 1.0
    pass = false;
    try
    {
      c = new Color(1.2f, 0.4f, 0.6f);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
 
    // green > 1.0
    pass = false;
    try
    {
      c = new Color(0.2f, 1.4f, 0.6f);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
 
    // blue > 1.0
    pass = false;
    try
    {
      c = new Color(0.2f, 0.4f, 1.6f);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
 
  }

  private void testConstructor3(TestHarness harness) 
  {
    harness.checkPoint("(float, float, float, float)");
    Color c = new Color(0.2f, 0.4f, 0.6f, 0.8f);
    harness.check(c.getRed(), 51);
    harness.check(c.getGreen(), 102);
    harness.check(c.getBlue(), 153);
    harness.check(c.getAlpha(), 204);
 
    // negative red
    boolean pass = false;
    try
    {
      c = new Color(-0.2f, 0.4f, 0.6f, 0.8f);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
 
    // negative green
    pass = false;
    try
    {
      c = new Color(0.2f, -0.4f, 0.6f, 0.8f);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
     
    // negative blue
    pass = false;
    try
    {
      c = new Color(0.2f, 0.4f, -0.6f, 0.8f);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
 
    // negative alpha
    pass = false;
    try
    {
      c = new Color(0.2f, 0.4f, 0.6f, -0.8f);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
 
    // red > 1.0
    pass = false;
    try
    {
      c = new Color(1.2f, 0.4f, 0.6f, 0.8f);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
 
    // green > 1.0
    pass = false;
    try
    {
      c = new Color(0.2f, 1.4f, 0.6f, 0.8f);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
 
    // blue > 1.0
    pass = false;
    try
    {
      c = new Color(0.2f, 0.4f, 1.6f, 0.8f);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
 
    // alpha > 1.0
    pass = false;
    try
    {
      c = new Color(0.2f, 0.4f, 0.6f, 1.8f);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
 
  }

  private void testConstructor4(TestHarness harness) 
  {
    harness.checkPoint("(int)");
    Color c = new Color(0x12345678);
    harness.check(c.getRed(), 0x34);
    harness.check(c.getGreen(), 0x56);
    harness.check(c.getBlue(), 0x78);
    harness.check(c.getAlpha(), 0xFF);
  }
  
  private void testConstructor5(TestHarness harness) 
  {
    harness.checkPoint("(int, boolean)");
    Color c = new Color(0x12345678, false);
    harness.check(c.getRed(), 0x34);
    harness.check(c.getGreen(), 0x56);
    harness.check(c.getBlue(), 0x78);
    harness.check(c.getAlpha(), 0xFF);
    
    c = new Color(0x12345678, true);
    harness.check(c.getRed(), 0x34);
    harness.check(c.getGreen(), 0x56);
    harness.check(c.getBlue(), 0x78);
    harness.check(c.getAlpha(), 0x12);
  }
  
  private void testConstructor6(TestHarness harness) 
  {
    harness.checkPoint("(int, int, int)");
    Color c = new Color(12, 34, 56);
    harness.check(c.getRed(), 12);
    harness.check(c.getGreen(), 34);
    harness.check(c.getBlue(), 56);
    harness.check(c.getAlpha(), 255);
 
    // negative red
    boolean pass = false;
    try
    {
      c = new Color(-12, 34, 56);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
 
    // negative green
    pass = false;
    try
    {
      c = new Color(12, -34, 56);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
     
    // negative blue
    pass = false;
    try
    {
      c = new Color(12, 34, -56);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
 
    // red > 1.0
    pass = false;
    try
    {
      c = new Color(512, 34, 56);
    }
    catch (IllegalArgumentException e)  
    {
      pass = true;   
    }
    harness.check(pass);
 
    // green > 1.0
    pass = false;
    try  
    {
      c = new Color(12, 534, 56);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    } 
    harness.check(pass);
 
    // blue > 1.0
    pass = false;
    try
    {
      c = new Color(12, 34, 556);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
  }

  private void testConstructor7(TestHarness harness) 
  {
    harness.checkPoint("(int, int, int, int)");
    Color c = new Color(12, 34, 56, 78);
    harness.check(c.getRed(), 12);
    harness.check(c.getGreen(), 34);
    harness.check(c.getBlue(), 56);
    harness.check(c.getAlpha(), 78);
 
    // negative red
    boolean pass = false;
    try
    {
      c = new Color(-12, 34, 56, 78);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
 
    // negative green
    pass = false;
    try
    {
      c = new Color(12, -34, 56, 78);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
     
    // negative blue
    pass = false;
    try
    {
      c = new Color(12, 34, -56, 78);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
 
    // negative alpha
    pass = false;
    try
    {
      c = new Color(12, 34, 56, -78);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
 
    // red > 1.0
    pass = false;
    try
    {
      c = new Color(512, 34, 56, 78);
    }
    catch (IllegalArgumentException e)  
    {
      pass = true;   
    }
    harness.check(pass);
 
    // green > 1.0
    pass = false;
    try  
    {
      c = new Color(12, 534, 56, 78);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    } 
    harness.check(pass);
 
    // blue > 1.0
    pass = false;
    try
    {
      c = new Color(12, 34, 556, 78);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);

    // alpha > 1.0
    pass = false;
    try
    {
      c = new Color(12, 34, 56, 578);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
  
  }

}
