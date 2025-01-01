/* constructors.java -- some checks for the constructors in the BufferedImage
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

// Tags: JDK1.4

package gnu.testlet.java.awt.image.BufferedImage;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.color.ColorSpace;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.ComponentColorModel;

public class constructors implements Testlet 
{
  public void test(TestHarness harness)
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
  }
 
  public void testConstructor1(TestHarness harness) 
  {
    harness.checkPoint("(int, int, int)");
    
    // TYPE_BYTE_GRAY
    BufferedImage image = new BufferedImage(10, 20, 
            BufferedImage.TYPE_BYTE_GRAY);
    harness.check(image.getWidth(), 10);
    harness.check(image.getHeight(), 20);
    harness.check(image.getType(), BufferedImage.TYPE_BYTE_GRAY);
    ColorModel cm = image.getColorModel();
    harness.check(cm instanceof ComponentColorModel);
    harness.check(cm.getColorSpace(), 
            ColorSpace.getInstance(ColorSpace.CS_GRAY));
    
    // TYPE_USHORT_GRAY
    image = new BufferedImage(10, 20, BufferedImage.TYPE_USHORT_GRAY);
    harness.check(image.getWidth(), 10);
    harness.check(image.getHeight(), 20);
    harness.check(image.getType(), BufferedImage.TYPE_USHORT_GRAY);
    cm = image.getColorModel();
    harness.check(cm instanceof ComponentColorModel);
    harness.check(cm.getColorSpace(), 
            ColorSpace.getInstance(ColorSpace.CS_GRAY));
    
    // try zero width
    boolean pass = false;
    try
    {
      image = new BufferedImage(0, 20, BufferedImage.TYPE_BYTE_GRAY);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // try zero height
    pass = false;
    try
    {
      image = new BufferedImage(10, 0, BufferedImage.TYPE_BYTE_GRAY);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // try bad type
    pass = false;
    try
    {
      image = new BufferedImage(10, 20, 666);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  public void testConstructor2(TestHarness harness) 
  {
    harness.checkPoint("(int, int, int, IndexColorModel)");
  }
  
  public void testConstructor3(TestHarness harness) 
  {
    harness.checkPoint("(ColorModel, WritableRaster, boolean, Hashtable)");
  }
}
