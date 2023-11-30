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

package gnu.testlet.java.awt.image.DirectColorModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Transparency;
import java.awt.color.ColorSpace;
import java.awt.image.DataBuffer;
import java.awt.image.DirectColorModel;

/**
 * Some checks for the constructors in the {@link DirectColorModel} class.
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
  }   

  private void testConstructor1(TestHarness harness) 
  {  
    harness.checkPoint("(ColorSpace, int, int, int, int, int, boolean, int)");
    DirectColorModel m1 = new DirectColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB), 
            32, 0xF000, 0xF00, 0xF0, 0xF, false, DataBuffer.TYPE_INT);
    harness.check(m1.getTransparency(), Transparency.TRANSLUCENT);
    
    // try null ColorSpace
    boolean pass = false;
    try
    {
      m1 = new DirectColorModel(null, 32, 0xF000, 0xF00, 0xF0, 0xF, false, 
            DataBuffer.TYPE_INT);
         
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // try non-RGB ColorSpace
    pass = false;
    try
    {
      m1 = new DirectColorModel(ColorSpace.getInstance(ColorSpace.CS_GRAY), 32, 
            0xF000, 0xF00, 0xF0, 0xF, false, DataBuffer.TYPE_INT);
         
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

  private void testConstructor2(TestHarness harness) 
  {  
    harness.checkPoint("(int, int, int, int)");

    // first an 8 bit model
    DirectColorModel m1 = new DirectColorModel(8, 0xF0, 0x0C, 0x03);
    harness.check(m1.getPixelSize(), 8);
    harness.check(m1.getComponentSize(0), 4);
    harness.check(m1.getComponentSize(1), 2);
    harness.check(m1.getComponentSize(2), 2);
    harness.check(m1.getTransparency(), Transparency.OPAQUE);
    harness.check(m1.getTransferType(), DataBuffer.TYPE_BYTE);
    
    // 16 bit model
    DirectColorModel m2 = new DirectColorModel(16, 0xFF00, 0xF0, 0x0F);
    harness.check(m2.getPixelSize(), 16);
    harness.check(m2.getComponentSize(0), 8);
    harness.check(m2.getComponentSize(1), 4);
    harness.check(m2.getComponentSize(2), 4);
    harness.check(m2.getTransparency(), Transparency.OPAQUE);
    harness.check(m2.getTransferType(), DataBuffer.TYPE_USHORT);
    
    // 32 bit model
    DirectColorModel m3 = new DirectColorModel(32, 0xFFFF00, 0xFF00, 0xFF);
    harness.check(m3.getPixelSize(), 32);
    harness.check(m3.getComponentSize(0), 16);
    harness.check(m3.getComponentSize(1), 8);
    harness.check(m3.getComponentSize(2), 8);
    harness.check(m3.getTransparency(), Transparency.OPAQUE);
    harness.check(m3.getTransferType(), DataBuffer.TYPE_INT);
    
    // check negative bits
    boolean pass = false;
    try
    {
      /* ColorModel m = */ new DirectColorModel(-1, 0xFFFF00, 0xFF00, 0xFF);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);

    // check bits > 32
    pass = false;
    try
    {
      /* ColorModel m = */ new DirectColorModel(33, 0xFFFF00, 0xFF00, 0xFF);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
  }

  private void testConstructor3(TestHarness harness) 
  {  
    harness.checkPoint("DirectColorModel(int, int, int, int, int)");

    // first an 8 bit model
    DirectColorModel m1 = new DirectColorModel(8, 0xC0, 0x30, 0x0C, 0x03);
    harness.check(m1.getPixelSize(), 8);
    harness.check(m1.getComponentSize(0), 2);
    harness.check(m1.getComponentSize(1), 2);
    harness.check(m1.getComponentSize(2), 2);
    harness.check(m1.getComponentSize(3), 2);
    harness.check(m1.getTransparency(), Transparency.TRANSLUCENT);
    harness.check(m1.getTransferType(), DataBuffer.TYPE_BYTE);
    
    // 16 bit model
    DirectColorModel m2 = new DirectColorModel(16, 0xF000, 0x0F00, 0xF0, 0x0F);
    harness.check(m2.getPixelSize(), 16);
    harness.check(m2.getComponentSize(0), 4);
    harness.check(m2.getComponentSize(1), 4);
    harness.check(m2.getComponentSize(2), 4);
    harness.check(m2.getComponentSize(3), 4);
    harness.check(m2.getTransparency(), Transparency.TRANSLUCENT);
    harness.check(m2.getTransferType(), DataBuffer.TYPE_USHORT);
    
    // 32 bit model
    DirectColorModel m3 = new DirectColorModel(32, 0xFF000000, 0xFF0000, 0xFF00, 0xFF);
    harness.check(m3.getPixelSize(), 32);
    harness.check(m3.getComponentSize(0), 8);
    harness.check(m3.getComponentSize(1), 8);
    harness.check(m3.getComponentSize(2), 8);
    harness.check(m3.getComponentSize(3), 8);
    harness.check(m3.getTransparency(), Transparency.TRANSLUCENT);
    harness.check(m3.getTransferType(), DataBuffer.TYPE_INT);
    
    // check negative bits
    boolean pass = false;
    try
    {
      /* ColorModel m = */ new DirectColorModel(-1, 0xFF000000, 0xFF0000, 0xFF00, 0xFF);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // check bits > 32
    pass = false;
    try
    {
      /* ColorModel m = */ new DirectColorModel(33, 0xFF000000, 0xFF0000, 0xFF00, 0xFF);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
  }

}
