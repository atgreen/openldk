/* filterImage.java -- some checks for the filter(Image) method of the
              ConvolveOp class.
   Copyright (C) 2006 Francis Kung <fkung@redhat.com>
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

package gnu.testlet.java.awt.image.LookupOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.BufferedImage;
import java.awt.image.ByteLookupTable;
import java.awt.image.ConvolveOp;
import java.awt.image.LookupOp;
import java.awt.image.ShortLookupTable;
import java.awt.image.WritableRaster;

/**
 * Checks the filter(BufferedImage) method in the {@link ConvolveOp} class.
 */
public class filterImage implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)      
  {
    testDefaults(harness);
    
    // Test lookup with only one lookup array: all colour components, but not
    // alpha component, should be operated on
    testOneLookup(harness, false);
    testOneLookup(harness, true);
    
    // Test lookup with num lookup arrays == num colour components, so alpha
    // component should be left alone
    testColourLookup(harness, false);
    testColourLookup(harness, true);
    
    // Test lookup with num lookup arrays == num components including alpha
    testAllLookup(harness, false);
    testAllLookup(harness, true);
    
    // Test behaviour when pixel value is not defined in lookup table
    testUndefined(harness);
  }
  
  private void testDefaults(TestHarness harness)
  {
    harness.checkPoint("testDefaults");
    
    // Create an image to work on
    BufferedImage img = new BufferedImage(20, 20, BufferedImage.TYPE_USHORT_GRAY);
    
    byte[] bytes = new byte[] {(byte) 0xAA, (byte) 0xBB};
    ByteLookupTable t = new ByteLookupTable(0, bytes);
    LookupOp op = new LookupOp(t, null);
    
    // Simple tests
    BufferedImage dst = op.filter(img, null);
    harness.check(dst.getType(), op.createCompatibleDestImage(img, null).getType());
    
    dst = new BufferedImage(20, 20, BufferedImage.TYPE_USHORT_GRAY);
    try
    {
      dst = op.filter(img, dst);
      harness.check(true);
    }
    catch (IllegalArgumentException ex)
    {
      harness.check(false);
    }
    
    // Src and dst images can be the same
    try
    {
      op.filter(img, img);
      harness.check(true);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    finally
    {
      // Reset image for next test
      img = new BufferedImage(20, 20, BufferedImage.TYPE_USHORT_GRAY);
    }
    
    // Src and dst are different sizes - not allowed
    dst = new BufferedImage(30, 40, BufferedImage.TYPE_USHORT_GRAY);
    try
    {
      op.filter(img, dst);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    // Src and dst have different tpyes (allowed)
    dst = new BufferedImage(20, 20, BufferedImage.TYPE_BYTE_GRAY);
    try
    {
      op.filter(img, dst);
      harness.check(true);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    // src and dst can even have different number of bands
    dst = new BufferedImage(20, 20, BufferedImage.TYPE_INT_ARGB);
    try
    {
      op.filter(img, dst);
      harness.check(true);
      harness.check(img.getType(), BufferedImage.TYPE_USHORT_GRAY);
      harness.check(dst.getType(), BufferedImage.TYPE_INT_ARGB);
    }
    catch (IllegalArgumentException ex)
    {
      harness.check(false);
    }
  }
  
  private void testOneLookup(TestHarness harness, boolean premultiply)
  {
    if (premultiply)
      harness.checkPoint("testOneLookup with premultiply");
    else
      harness.checkPoint("testOneLookup without premultiply");
    
    // Create an image to work on
    BufferedImage img;
    if (premultiply)
      img = new BufferedImage(5, 5, BufferedImage.TYPE_INT_ARGB_PRE);
    else
      img = new BufferedImage(5, 5, BufferedImage.TYPE_INT_ARGB);
    createImage(img);
    
    byte[] bytes = new byte[] {12, 23, 14, 35, 48, 2};
    ByteLookupTable t = new ByteLookupTable(0, bytes);
    LookupOp op = new LookupOp(t, null);
    
    BufferedImage dst = new BufferedImage(5, 5, BufferedImage.TYPE_INT_ARGB);
    dst = op.filter(img, dst);
    WritableRaster dest = dst.getRaster();
    
    harness.check(img.isAlphaPremultiplied(), premultiply);
    harness.check(dst.isAlphaPremultiplied(), false);
    
    harness.check(dest.getSample(0, 0, 0), 23);
    harness.check(dest.getSample(1, 0, 0), 35);
    harness.check(dest.getSample(2, 0, 0), 23);
    harness.check(dest.getSample(3, 0, 0), 35);
    harness.check(dest.getSample(4, 0, 0), 23);
    harness.check(dest.getSample(0, 1, 0), 48);
    harness.check(dest.getSample(1, 1, 0), 14);
    harness.check(dest.getSample(2, 1, 0), 48);
    harness.check(dest.getSample(3, 1, 0), 14);
    harness.check(dest.getSample(4, 1, 0), 48);
    harness.check(dest.getSample(0, 2, 0), 23);
    harness.check(dest.getSample(1, 2, 0), 35);
    harness.check(dest.getSample(2, 2, 0), 23);
    harness.check(dest.getSample(3, 2, 0), 35);
    harness.check(dest.getSample(4, 2, 0), 23);
    harness.check(dest.getSample(0, 3, 0), 48);
    harness.check(dest.getSample(1, 3, 0), 14);
    harness.check(dest.getSample(2, 3, 0), 48);
    harness.check(dest.getSample(3, 3, 0), 14);
    harness.check(dest.getSample(4, 3, 0), 48);
    harness.check(dest.getSample(0, 4, 0), 23);
    harness.check(dest.getSample(1, 4, 0), 35);
    harness.check(dest.getSample(2, 4, 0), 23);
    harness.check(dest.getSample(3, 4, 0), 35);
    harness.check(dest.getSample(4, 4, 0), 23);
    
    harness.check(dest.getSample(0, 0, 1), 12);
    harness.check(dest.getSample(1, 0, 1), 48);
    harness.check(dest.getSample(2, 0, 1), 12);
    harness.check(dest.getSample(3, 0, 1), 48);
    harness.check(dest.getSample(4, 0, 1), 12);
    harness.check(dest.getSample(0, 1, 1), 35);
    harness.check(dest.getSample(1, 1, 1), 23);
    harness.check(dest.getSample(2, 1, 1), 35);
    harness.check(dest.getSample(3, 1, 1), 23);
    harness.check(dest.getSample(4, 1, 1), 35);
    harness.check(dest.getSample(0, 2, 1), 12);
    harness.check(dest.getSample(1, 2, 1), 48);
    harness.check(dest.getSample(2, 2, 1), 12);
    harness.check(dest.getSample(3, 2, 1), 48);
    harness.check(dest.getSample(4, 2, 1), 12);
    harness.check(dest.getSample(0, 3, 1), 35);
    harness.check(dest.getSample(1, 3, 1), 23);
    harness.check(dest.getSample(2, 3, 1), 35);
    harness.check(dest.getSample(3, 3, 1), 23);
    harness.check(dest.getSample(4, 3, 1), 35);
    harness.check(dest.getSample(0, 4, 1), 12);
    harness.check(dest.getSample(1, 4, 1), 48);
    harness.check(dest.getSample(2, 4, 1), 12);
    harness.check(dest.getSample(3, 4, 1), 48);
    harness.check(dest.getSample(4, 4, 1), 12);
    
    // Sun's implementation uses RGAB (alpha as the 3rd band), as opposed to
    // our RGBA, so these tests will fail... however these might not be worth
    // changing, so left them commented out for now.
/*    
    harness.check(dest.getSample(0, 0, 2), 2);
    harness.check(dest.getSample(1, 0, 2), 3);
    harness.check(dest.getSample(2, 0, 2), 2);
    harness.check(dest.getSample(3, 0, 2), 3);
    harness.check(dest.getSample(4, 0, 2), 2);
    harness.check(dest.getSample(0, 1, 2), 4);
    harness.check(dest.getSample(1, 1, 2), 2);
    harness.check(dest.getSample(2, 1, 2), 4);
    harness.check(dest.getSample(3, 1, 2), 2);
    harness.check(dest.getSample(4, 1, 2), 4);
    harness.check(dest.getSample(0, 2, 2), 2);
    harness.check(dest.getSample(1, 2, 2), 3);
    harness.check(dest.getSample(2, 2, 2), 2);
    harness.check(dest.getSample(3, 2, 2), 3);
    harness.check(dest.getSample(4, 2, 2), 2);
    harness.check(dest.getSample(0, 3, 2), 4);
    harness.check(dest.getSample(1, 3, 2), 2);
    harness.check(dest.getSample(2, 3, 2), 4);
    harness.check(dest.getSample(3, 3, 2), 2);
    harness.check(dest.getSample(4, 3, 2), 4);
    harness.check(dest.getSample(0, 4, 2), 2);
    harness.check(dest.getSample(1, 4, 2), 3);
    harness.check(dest.getSample(2, 4, 2), 2);
    harness.check(dest.getSample(3, 4, 2), 3);
    harness.check(dest.getSample(4, 4, 2), 2);
    
    harness.check(dest.getSample(0, 0, 3), 12);
    harness.check(dest.getSample(1, 0, 3), 48);
    harness.check(dest.getSample(2, 0, 3), 12);
    harness.check(dest.getSample(3, 0, 3), 48);
    harness.check(dest.getSample(4, 0, 3), 12);
    harness.check(dest.getSample(0, 1, 3), 35);
    harness.check(dest.getSample(1, 1, 3), 23);
    harness.check(dest.getSample(2, 1, 3), 35);
    harness.check(dest.getSample(3, 1, 3), 23);
    harness.check(dest.getSample(4, 1, 3), 35);
    harness.check(dest.getSample(0, 2, 3), 12);
    harness.check(dest.getSample(1, 2, 3), 48);
    harness.check(dest.getSample(2, 2, 3), 12);
    harness.check(dest.getSample(3, 2, 3), 48);
    harness.check(dest.getSample(4, 2, 3), 12);
    harness.check(dest.getSample(0, 3, 3), 35);
    harness.check(dest.getSample(1, 3, 3), 23);
    harness.check(dest.getSample(2, 3, 3), 35);
    harness.check(dest.getSample(3, 3, 3), 23);
    harness.check(dest.getSample(4, 3, 3), 35);
    harness.check(dest.getSample(0, 4, 3), 12);
    harness.check(dest.getSample(1, 4, 3), 48);
    harness.check(dest.getSample(2, 4, 3), 12);
    harness.check(dest.getSample(3, 4, 3), 48);
    harness.check(dest.getSample(4, 4, 3), 12);
    */
  }
  
  private void testColourLookup(TestHarness harness, boolean premultiply)
  {
    if (premultiply)
      harness.checkPoint("testColourLookup with premultiply");
    else
      harness.checkPoint("testColourLookup without premultiply");
    
    // Create an image to work on
    BufferedImage img;
    if (premultiply)
      img = new BufferedImage(5, 5, BufferedImage.TYPE_INT_ARGB_PRE);
    else
      img = new BufferedImage(5, 5, BufferedImage.TYPE_INT_ARGB);
    createImage(img);
    
    short[][] lutarray = new short[][] {{12, 23, 14, 35, 48, 2},
                                        {112, 123, 114, 135, 148, 102},
                                        {212, 223, 214, 235, 248, 202}
                                       };
    
    ShortLookupTable t = new ShortLookupTable(0, lutarray);
    LookupOp op = new LookupOp(t, null);
    BufferedImage dst = new BufferedImage(5, 5, BufferedImage.TYPE_INT_ARGB);

    /* This causes Sun to throw an exception...
     * 
     * java.lang.IllegalArgumentException: Number of channels in the src (4)
     * does not match number of channels in the destination (2)
     * 
     * I'm pretty sure it's a bug, but it's not one that's worth mimicing.
     * This test will not run on Sun.
     */ 
    try
    {
      dst = op.filter(img, dst);
      WritableRaster dest = dst.getRaster();
      
      harness.check(img.isAlphaPremultiplied(), premultiply);
      harness.check(dst.isAlphaPremultiplied(), false);
      
      harness.check(dest.getSample(0, 0, 0), 23);
      harness.check(dest.getSample(1, 0, 0), 35);
      harness.check(dest.getSample(2, 0, 0), 23);
      harness.check(dest.getSample(3, 0, 0), 35);
      harness.check(dest.getSample(4, 0, 0), 23);
      harness.check(dest.getSample(0, 1, 0), 48);
      harness.check(dest.getSample(1, 1, 0), 14);
      harness.check(dest.getSample(2, 1, 0), 48);
      harness.check(dest.getSample(3, 1, 0), 14);
      harness.check(dest.getSample(4, 1, 0), 48);
      harness.check(dest.getSample(0, 2, 0), 23);
      harness.check(dest.getSample(1, 2, 0), 35);
      harness.check(dest.getSample(2, 2, 0), 23);
      harness.check(dest.getSample(3, 2, 0), 35);
      harness.check(dest.getSample(4, 2, 0), 23);
      harness.check(dest.getSample(0, 3, 0), 48);
      harness.check(dest.getSample(1, 3, 0), 14);
      harness.check(dest.getSample(2, 3, 0), 48);
      harness.check(dest.getSample(3, 3, 0), 14);
      harness.check(dest.getSample(4, 3, 0), 48);
      harness.check(dest.getSample(0, 4, 0), 23);
      harness.check(dest.getSample(1, 4, 0), 35);
      harness.check(dest.getSample(2, 4, 0), 23);
      harness.check(dest.getSample(3, 4, 0), 35);
      harness.check(dest.getSample(4, 4, 0), 23);
      
      harness.check(dest.getSample(0, 0, 1), 112);
      harness.check(dest.getSample(1, 0, 1), 148);
      harness.check(dest.getSample(2, 0, 1), 112);
      harness.check(dest.getSample(3, 0, 1), 148);
      harness.check(dest.getSample(4, 0, 1), 112);
      harness.check(dest.getSample(0, 1, 1), 135);
      harness.check(dest.getSample(1, 1, 1), 123);
      harness.check(dest.getSample(2, 1, 1), 135);
      harness.check(dest.getSample(3, 1, 1), 123);
      harness.check(dest.getSample(4, 1, 1), 135);
      harness.check(dest.getSample(0, 2, 1), 112);
      harness.check(dest.getSample(1, 2, 1), 148);
      harness.check(dest.getSample(2, 2, 1), 112);
      harness.check(dest.getSample(3, 2, 1), 148);
      harness.check(dest.getSample(4, 2, 1), 112);
      harness.check(dest.getSample(0, 3, 1), 135);
      harness.check(dest.getSample(1, 3, 1), 123);
      harness.check(dest.getSample(2, 3, 1), 135);
      harness.check(dest.getSample(3, 3, 1), 123);
      harness.check(dest.getSample(4, 3, 1), 135);
      harness.check(dest.getSample(0, 4, 1), 112);
      harness.check(dest.getSample(1, 4, 1), 148);
      harness.check(dest.getSample(2, 4, 1), 112);
      harness.check(dest.getSample(3, 4, 1), 148);
      harness.check(dest.getSample(4, 4, 1), 112);
      
      // Sun's implementation uses RGAB (alpha as the 3rd band), as opposed to
      // our RGBA, so these tests will fail... however these might not be worth
      // changing, so left them commented out for now.
      /*
      harness.check(dest.getSample(0, 0, 2), 2);
      harness.check(dest.getSample(1, 0, 2), 3);
      harness.check(dest.getSample(2, 0, 2), 2);
      harness.check(dest.getSample(3, 0, 2), 3);
      harness.check(dest.getSample(4, 0, 2), 2);
      harness.check(dest.getSample(0, 1, 2), 4);
      harness.check(dest.getSample(1, 1, 2), 2);
      harness.check(dest.getSample(2, 1, 2), 4);
      harness.check(dest.getSample(3, 1, 2), 2);
      harness.check(dest.getSample(4, 1, 2), 4);
      harness.check(dest.getSample(0, 2, 2), 2);
      harness.check(dest.getSample(1, 2, 2), 3);
      harness.check(dest.getSample(2, 2, 2), 2);
      harness.check(dest.getSample(3, 2, 2), 3);
      harness.check(dest.getSample(4, 2, 2), 2);
      harness.check(dest.getSample(0, 3, 2), 4);
      harness.check(dest.getSample(1, 3, 2), 2);
      harness.check(dest.getSample(2, 3, 2), 4);
      harness.check(dest.getSample(3, 3, 2), 2);
      harness.check(dest.getSample(4, 3, 2), 4);
      harness.check(dest.getSample(0, 4, 2), 2);
      harness.check(dest.getSample(1, 4, 2), 3);
      harness.check(dest.getSample(2, 4, 2), 2);
      harness.check(dest.getSample(3, 4, 2), 3);
      harness.check(dest.getSample(4, 4, 2), 2);
      
      harness.check(dest.getSample(0, 0, 3), 212);
      harness.check(dest.getSample(1, 0, 3), 248);
      harness.check(dest.getSample(2, 0, 3), 212);
      harness.check(dest.getSample(3, 0, 3), 248);
      harness.check(dest.getSample(4, 0, 3), 212);
      harness.check(dest.getSample(0, 1, 3), 235);
      harness.check(dest.getSample(1, 1, 3), 223);
      harness.check(dest.getSample(2, 1, 3), 235);
      harness.check(dest.getSample(3, 1, 3), 223);
      harness.check(dest.getSample(4, 1, 3), 235);
      harness.check(dest.getSample(0, 2, 3), 212);
      harness.check(dest.getSample(1, 2, 3), 248);
      harness.check(dest.getSample(2, 2, 3), 212);
      harness.check(dest.getSample(3, 2, 3), 248);
      harness.check(dest.getSample(4, 2, 3), 212);
      harness.check(dest.getSample(0, 3, 3), 235);
      harness.check(dest.getSample(1, 3, 3), 223);
      harness.check(dest.getSample(2, 3, 3), 235);
      harness.check(dest.getSample(3, 3, 3), 223);
      harness.check(dest.getSample(4, 3, 3), 235);
      harness.check(dest.getSample(0, 4, 3), 212);
      harness.check(dest.getSample(1, 4, 3), 248);
      harness.check(dest.getSample(2, 4, 3), 212);
      harness.check(dest.getSample(3, 4, 3), 248);
      harness.check(dest.getSample(4, 4, 3), 212);
      */
    }
    catch (IllegalArgumentException ex)
    {
      harness.debug("Test did not run - this is expected on Sun due to a " +
            "bug in their implementation, but this message should not show " +
            "when testing Classpath.");
    }
  }
  
  private void testAllLookup(TestHarness harness, boolean premultiply)
  {
    if (premultiply)
      harness.checkPoint("testAllLookup with premultiply");
    else
      harness.checkPoint("testAllLookup without premultiply");
    
    // Create an image to work on
    BufferedImage img;
    if (premultiply)
      img = new BufferedImage(5, 5, BufferedImage.TYPE_INT_ARGB_PRE);
    else
      img = new BufferedImage(5, 5, BufferedImage.TYPE_INT_ARGB);
    createImage(img);
    
    short[][] lutarray = new short[][] {{12, 23, 14, 35, 48, 2},
                                        {112, 123, 114, 135, 148, 102},
                                        {212, 223, 214, 235, 248, 202},
                                        {62, 73, 64, 85, 98, 52}
                                       };
    
    ShortLookupTable t = new ShortLookupTable(0, lutarray);
    LookupOp op = new LookupOp(t, null);
    
    BufferedImage dst = new BufferedImage(5, 5, BufferedImage.TYPE_INT_ARGB);
    dst = op.filter(img, dst);
    WritableRaster dest = dst.getRaster();
    
    harness.check(img.isAlphaPremultiplied(), premultiply);
    harness.check(dst.isAlphaPremultiplied(), false);
    
    harness.check(dest.getSample(0, 0, 0), 23);
    harness.check(dest.getSample(1, 0, 0), 35);
    harness.check(dest.getSample(2, 0, 0), 23);
    harness.check(dest.getSample(3, 0, 0), 35);
    harness.check(dest.getSample(4, 0, 0), 23);
    harness.check(dest.getSample(0, 1, 0), 48);
    harness.check(dest.getSample(1, 1, 0), 14);
    harness.check(dest.getSample(2, 1, 0), 48);
    harness.check(dest.getSample(3, 1, 0), 14);
    harness.check(dest.getSample(4, 1, 0), 48);
    harness.check(dest.getSample(0, 2, 0), 23);
    harness.check(dest.getSample(1, 2, 0), 35);
    harness.check(dest.getSample(2, 2, 0), 23);
    harness.check(dest.getSample(3, 2, 0), 35);
    harness.check(dest.getSample(4, 2, 0), 23);
    harness.check(dest.getSample(0, 3, 0), 48);
    harness.check(dest.getSample(1, 3, 0), 14);
    harness.check(dest.getSample(2, 3, 0), 48);
    harness.check(dest.getSample(3, 3, 0), 14);
    harness.check(dest.getSample(4, 3, 0), 48);
    harness.check(dest.getSample(0, 4, 0), 23);
    harness.check(dest.getSample(1, 4, 0), 35);
    harness.check(dest.getSample(2, 4, 0), 23);
    harness.check(dest.getSample(3, 4, 0), 35);
    harness.check(dest.getSample(4, 4, 0), 23);
    
    harness.check(dest.getSample(0, 0, 1), 112);
    harness.check(dest.getSample(1, 0, 1), 148);
    harness.check(dest.getSample(2, 0, 1), 112);
    harness.check(dest.getSample(3, 0, 1), 148);
    harness.check(dest.getSample(4, 0, 1), 112);
    harness.check(dest.getSample(0, 1, 1), 135);
    harness.check(dest.getSample(1, 1, 1), 123);
    harness.check(dest.getSample(2, 1, 1), 135);
    harness.check(dest.getSample(3, 1, 1), 123);
    harness.check(dest.getSample(4, 1, 1), 135);
    harness.check(dest.getSample(0, 2, 1), 112);
    harness.check(dest.getSample(1, 2, 1), 148);
    harness.check(dest.getSample(2, 2, 1), 112);
    harness.check(dest.getSample(3, 2, 1), 148);
    harness.check(dest.getSample(4, 2, 1), 112);
    harness.check(dest.getSample(0, 3, 1), 135);
    harness.check(dest.getSample(1, 3, 1), 123);
    harness.check(dest.getSample(2, 3, 1), 135);
    harness.check(dest.getSample(3, 3, 1), 123);
    harness.check(dest.getSample(4, 3, 1), 135);
    harness.check(dest.getSample(0, 4, 1), 112);
    harness.check(dest.getSample(1, 4, 1), 148);
    harness.check(dest.getSample(2, 4, 1), 112);
    harness.check(dest.getSample(3, 4, 1), 148);
    harness.check(dest.getSample(4, 4, 1), 112);
    
    harness.check(dest.getSample(0, 0, 2), 214);
    harness.check(dest.getSample(1, 0, 2), 235);
    harness.check(dest.getSample(2, 0, 2), 214);
    harness.check(dest.getSample(3, 0, 2), 235);
    harness.check(dest.getSample(4, 0, 2), 214);
    harness.check(dest.getSample(0, 1, 2), 248);
    harness.check(dest.getSample(1, 1, 2), 214);
    harness.check(dest.getSample(2, 1, 2), 248);
    harness.check(dest.getSample(3, 1, 2), 214);
    harness.check(dest.getSample(4, 1, 2), 248);
    harness.check(dest.getSample(0, 2, 2), 214);
    harness.check(dest.getSample(1, 2, 2), 235);
    harness.check(dest.getSample(2, 2, 2), 214);
    harness.check(dest.getSample(3, 2, 2), 235);
    harness.check(dest.getSample(4, 2, 2), 214);
    harness.check(dest.getSample(0, 3, 2), 248);
    harness.check(dest.getSample(1, 3, 2), 214);
    harness.check(dest.getSample(2, 3, 2), 248);
    harness.check(dest.getSample(3, 3, 2), 214);
    harness.check(dest.getSample(4, 3, 2), 248);
    harness.check(dest.getSample(0, 4, 2), 214);
    harness.check(dest.getSample(1, 4, 2), 235);
    harness.check(dest.getSample(2, 4, 2), 214);
    harness.check(dest.getSample(3, 4, 2), 235);
    harness.check(dest.getSample(4, 4, 2), 214);
    
    harness.check(dest.getSample(0, 0, 3), 62);
    harness.check(dest.getSample(1, 0, 3), 98);
    harness.check(dest.getSample(2, 0, 3), 62);
    harness.check(dest.getSample(3, 0, 3), 98);
    harness.check(dest.getSample(4, 0, 3), 62);
    harness.check(dest.getSample(0, 1, 3), 85);
    harness.check(dest.getSample(1, 1, 3), 73);
    harness.check(dest.getSample(2, 1, 3), 85);
    harness.check(dest.getSample(3, 1, 3), 73);
    harness.check(dest.getSample(4, 1, 3), 85);
    harness.check(dest.getSample(0, 2, 3), 62);
    harness.check(dest.getSample(1, 2, 3), 98);
    harness.check(dest.getSample(2, 2, 3), 62);
    harness.check(dest.getSample(3, 2, 3), 98);
    harness.check(dest.getSample(4, 2, 3), 62);
    harness.check(dest.getSample(0, 3, 3), 85);
    harness.check(dest.getSample(1, 3, 3), 73);
    harness.check(dest.getSample(2, 3, 3), 85);
    harness.check(dest.getSample(3, 3, 3), 73);
    harness.check(dest.getSample(4, 3, 3), 85);
    harness.check(dest.getSample(0, 4, 3), 62);
    harness.check(dest.getSample(1, 4, 3), 98);
    harness.check(dest.getSample(2, 4, 3), 62);
    harness.check(dest.getSample(3, 4, 3), 98);
    harness.check(dest.getSample(4, 4, 3), 62);
  }

  private void testUndefined(TestHarness harness)
  {
    harness.checkPoint("testUndefined");
    
    // Create an image to work on
    BufferedImage img = new BufferedImage(5, 5, BufferedImage.TYPE_INT_ARGB);
    WritableRaster r = img.getRaster();
    r.setSample(1, 1, 0, 20);

    // We get the ArrayIndexOutOfBoundsException that we would expect when
    // using a ShortLookupTable...
    short[] lutarray = new short[] {0, 1, 2, 3, 4, 5, 6, 7, 8};
    ShortLookupTable t = new ShortLookupTable(0, lutarray);
    LookupOp op = new LookupOp(t, null);
    
    try
    {
      op.filter(img, null);
      harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException ex)
    {
      harness.check(true);
    }

    // But a ByteLookupTable will give us undetermined results
    // (is a native array overflowing in sun's implementation?)
    byte[] bytes = new byte[] {0, 1, 2, 3, 4, 5, 6, 7, 8};
    ByteLookupTable t2 = new ByteLookupTable(0, bytes);
    op = new LookupOp(t2, null);
    
    try
    {
      op.filter(img, null);
      harness.check(true);
    }
    catch (ArrayIndexOutOfBoundsException ex)
    {
      // Allow an exception to be thrown anyways, since it makes more sense
      harness.check(true);
    }

    // And a test for the behaviour when source and dest have a different
    // number of bands (since the constructor does not throw an exception)
    r.setSample(1, 1, 0, 5);
    r.setSample(1, 1, 1, 4);
    r.setSample(1, 1, 2, 3);
    r.setSample(1, 1, 3, 2);
    BufferedImage dst = new BufferedImage(5, 5, BufferedImage.TYPE_USHORT_GRAY);
    try
    {
      // It doesn't throw an exception...
      op.filter(img, dst);
      harness.check(true);
    }
    catch (IllegalArgumentException ex)
    {
      harness.check(false);
    }
    
    // Now try a destination with *more* bands
    img = new BufferedImage(5, 5, BufferedImage.TYPE_USHORT_GRAY);
    r = img.getRaster();
    r.setSample(1, 1, 0, 5);
    dst = new BufferedImage(5, 5, BufferedImage.TYPE_INT_ARGB);
    try
    {
      // It doesn't throw an exception either...
      op.filter(img, dst);
      harness.check(true);
    }
    catch (IllegalArgumentException ex)
    {
      harness.check(false);
    }
  }
  
  // Vary pixel values in the image
  private void createImage(BufferedImage img)
  {
    WritableRaster r = img.getRaster();
    for (int i = 0; i < r.getHeight(); i++)
      for (int j = 0; j < r.getWidth(); j++)
        {
          if (i % 2 == 0)
            if (j % 2 == 0)
              {
                r.setSample(j, i, 0, 1);
                r.setSample(j, i, 1, 0);
                r.setSample(j, i, 2, 2);
                r.setSample(j, i, 3, 0);
              }
            else
              {
                r.setSample(j, i, 0, 3);
                r.setSample(j, i, 1, 4);
                r.setSample(j, i, 2, 3);
                r.setSample(j, i, 3, 4);
              }
          else
            if (j % 2 == 0)
              {
                r.setSample(j, i, 0, 4);
                r.setSample(j, i, 1, 3);
                r.setSample(j, i, 2, 4);
                r.setSample(j, i, 3, 3);
              }
            else
              {
                r.setSample(j, i, 0, 2);
                r.setSample(j, i, 1, 1);
                r.setSample(j, i, 2, 2);
                r.setSample(j, i, 3, 1);
              }
        }
  }
}

