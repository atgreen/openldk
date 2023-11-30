/* constructors.java -- some checks for the constructors in the ColorModel 
       class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.2
// Uses: MyColorModel

package gnu.testlet.java.awt.image.ColorModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Transparency;
import java.awt.color.ColorSpace;
import java.awt.image.DataBuffer;

public class constructors implements Testlet
{
  public void test(TestHarness harness)
  {
    testConstructor1(harness);
    testConstructor2(harness);
  }
  
  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("(int)");
    MyColorModel cm = new MyColorModel(8);
    harness.check(cm.getNumColorComponents(), 3);
    harness.check(cm.getNumComponents(), 4);
    harness.check(cm.getPixelSize(), 8);
    harness.check(cm.getColorSpace(), ColorSpace.getInstance(
            ColorSpace.CS_sRGB));
    harness.check(cm.isAlphaPremultiplied(), false);
    harness.check(cm.getTransparency(), Transparency.TRANSLUCENT);
    harness.check(cm.getTransferType(), DataBuffer.TYPE_BYTE);
    
    cm = new MyColorModel(16);
    harness.check(cm.getNumColorComponents(), 3);
    harness.check(cm.getNumComponents(), 4);
    harness.check(cm.getPixelSize(), 16);
    harness.check(cm.getColorSpace(), ColorSpace.getInstance(
            ColorSpace.CS_sRGB));
    harness.check(cm.isAlphaPremultiplied(), false);
    harness.check(cm.getTransparency(), Transparency.TRANSLUCENT);
    harness.check(cm.getTransferType(), DataBuffer.TYPE_USHORT);

    cm = new MyColorModel(32);
    harness.check(cm.getNumColorComponents(), 3);
    harness.check(cm.getNumComponents(), 4);
    harness.check(cm.getPixelSize(), 32);
    harness.check(cm.getColorSpace(), ColorSpace.getInstance(
            ColorSpace.CS_sRGB));
    harness.check(cm.isAlphaPremultiplied(), false);
    harness.check(cm.getTransparency(), Transparency.TRANSLUCENT);
    harness.check(cm.getTransferType(), DataBuffer.TYPE_INT);

    cm = new MyColorModel(64);
    harness.check(cm.getNumColorComponents(), 3);
    harness.check(cm.getNumComponents(), 4);
    harness.check(cm.getPixelSize(), 64);
    harness.check(cm.getColorSpace(), ColorSpace.getInstance(
            ColorSpace.CS_sRGB));
    harness.check(cm.isAlphaPremultiplied(), false);
    harness.check(cm.getTransparency(), Transparency.TRANSLUCENT);
    harness.check(cm.getTransferType(), DataBuffer.TYPE_UNDEFINED);

    boolean pass = false;
    try
    {
      cm = new MyColorModel(0);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  public void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(int, int[], ColorSpace, boolean, boolean, int, int)");    
    MyColorModel cm = new MyColorModel(96, new int[] {32, 32, 32}, 
            ColorSpace.getInstance(ColorSpace.CS_sRGB), false, false, 
            Transparency.OPAQUE, DataBuffer.TYPE_INT);
    harness.check(cm.getNumColorComponents(), 3);
    harness.check(cm.getNumComponents(), 3);
    harness.check(cm.getPixelSize(), 96);
    harness.check(cm.getColorSpace(), ColorSpace.getInstance(ColorSpace.CS_sRGB));
    harness.check(cm.isAlphaPremultiplied(), false);
    harness.check(cm.getTransparency(), Transparency.OPAQUE);
    harness.check(cm.getTransferType(), DataBuffer.TYPE_INT);
    
    // try 0 for pixel_bits
    boolean pass = false;
    try
    {
      cm = new MyColorModel(0, new int[] {32, 32, 32}, 
            ColorSpace.getInstance(ColorSpace.CS_sRGB), false, false, 
            Transparency.OPAQUE, DataBuffer.TYPE_INT);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try null bits array
    pass = false;
    try
    {
      cm = new MyColorModel(96, null, 
              ColorSpace.getInstance(ColorSpace.CS_sRGB), false, false, 
              Transparency.OPAQUE, DataBuffer.TYPE_INT);      
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try a null ColorSpace
    pass = false;
    try
    {
      cm = new MyColorModel(96, new int[] {32, 32, 32}, 
            null, false, false, 
            Transparency.OPAQUE, DataBuffer.TYPE_INT);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try a bad transparency
    pass = false;
    try
    {
      cm = new MyColorModel(96, new int[] {32, 32, 32}, 
            ColorSpace.getInstance(ColorSpace.CS_sRGB), false, false, 
            -1, DataBuffer.TYPE_INT);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
