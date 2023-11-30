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

package gnu.testlet.javax.swing.plaf.ColorUIResource;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;

import javax.swing.plaf.ColorUIResource;

/**
 * Some checks for the constructors in the {@link ColorUIResource} class.  
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
    harness.checkPoint("(Color)");
    ColorUIResource c1 = new ColorUIResource(Color.blue);
    harness.check(c1.getRGB() == Color.blue.getRGB());
    
    // check null argument
    boolean pass = false;
    try
    {
      /* ColorUIResource c = */ new ColorUIResource(null);   
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
    ColorUIResource c = new ColorUIResource(0.2f, 0.4f, 0.6f);
    harness.check(c.getRed(), 51);
    harness.check(c.getGreen(), 102);
    harness.check(c.getBlue(), 153);
    harness.check(c.getAlpha(), 255);
    
    // negative red
    boolean pass = false;
    try
    {
      c = new ColorUIResource(-0.2f, 0.4f, 0.6f);
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
      c = new ColorUIResource(0.2f, -0.4f, 0.6f);
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
      c = new ColorUIResource(0.2f, 0.4f, -0.6f);
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
      c = new ColorUIResource(1.2f, 0.4f, 0.6f);
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
      c = new ColorUIResource(0.2f, 1.4f, 0.6f);
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
      c = new ColorUIResource(0.2f, 0.4f, 1.6f);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
  }

  private void testConstructor3(TestHarness harness) 
  {
    harness.checkPoint("(int)");
    ColorUIResource c = new ColorUIResource(0x12345678);
    harness.check(c.getRed(), 0x34);
    harness.check(c.getGreen(), 0x56);
    harness.check(c.getBlue(), 0x78);
    harness.check(c.getAlpha(), 255);
  }

  private void testConstructor4(TestHarness harness) 
  {
    harness.checkPoint("(int, int, int)");
    ColorUIResource c = new ColorUIResource(12, 34, 56);
    harness.check(c.getRed(), 12);
    harness.check(c.getGreen(), 34);
    harness.check(c.getBlue(), 56);
    harness.check(c.getAlpha(), 255);
    
    // negative red
    boolean pass = false;
    try
    {
      c = new ColorUIResource(-12, 34, 56);
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
      c = new ColorUIResource(12, -34, 56);
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
      c = new ColorUIResource(12, 34, -56);
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
      c = new ColorUIResource(512, 34, 56);
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
      c = new ColorUIResource(12, 534, 56);
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
      c = new ColorUIResource(12, 34, 556);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
  }

}
