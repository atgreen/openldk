/* createCompatibleDestImage.java -- some checks for the
       createCompatibleDestImage() method of the LookupOp class.
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
import java.awt.image.DataBuffer;
import java.awt.image.DirectColorModel;
import java.awt.image.LookupOp;

/**
 * Checks for the createCompatibleDestImage method in the
 * {@link LookupOp} class.
 */
public class createCompatibleDestImage implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)      
  {
    simpleTest(harness);
    colorModelTest(harness);
  }
  
  private void simpleTest(TestHarness harness)
  {
    harness.checkPoint("createCompatibleDestImage");

    // Simple test
    byte[] bytes = new byte[] {(byte) 0xAA, (byte) 0xBB};
    ByteLookupTable t = new ByteLookupTable(0, bytes);
    LookupOp op = new LookupOp(t, null);
    
    BufferedImage img = new BufferedImage(25, 40, BufferedImage.TYPE_INT_RGB);
    
    BufferedImage dest = op.createCompatibleDestImage(img, img.getColorModel());
    
    harness.check(dest.getHeight(), 40);
    harness.check(dest.getWidth(), 25);
    harness.check(dest.getColorModel(), img.getColorModel());
    
    // Try a different colour model
    img = new BufferedImage(25, 40, BufferedImage.TYPE_INT_RGB);
    DirectColorModel cm = new DirectColorModel(16, 0x0f00, 0x00f0, 0x000f);
    dest = op.createCompatibleDestImage(img, cm);
    
    harness.check(dest.getHeight(), 40);
    harness.check(dest.getWidth(), 25);
    harness.check(dest.getColorModel(), cm);
  }
  
  // Test all the default color models
  private void colorModelTest(TestHarness harness)
  {
    byte[] bytes = new byte[] {(byte) 0xAA, (byte) 0xBB};
    ByteLookupTable t = new ByteLookupTable(0, bytes);
    LookupOp op = new LookupOp(t, null);

    int[] types = {BufferedImage.TYPE_INT_RGB,
                   BufferedImage.TYPE_INT_ARGB,
                   BufferedImage.TYPE_INT_ARGB_PRE,
                   BufferedImage.TYPE_3BYTE_BGR,
                   BufferedImage.TYPE_4BYTE_ABGR,
                   BufferedImage.TYPE_4BYTE_ABGR_PRE,
                   BufferedImage.TYPE_USHORT_565_RGB,
                   BufferedImage.TYPE_USHORT_555_RGB,
                   BufferedImage.TYPE_BYTE_GRAY,
                   BufferedImage.TYPE_USHORT_GRAY};
    // Skipped types that are not implemented yet:
    // TYPE_CUSTOM, TYPE_INT_BGR, TYPE_BYTE_BINARY, TYPE_BYTE_INDEXED

    for (int i = 0; i < types.length; i++)
      {
        int type = types[i];
        harness.checkPoint("type: " + type);
        
        BufferedImage img = new BufferedImage(25, 40, type);
        BufferedImage dest = op.createCompatibleDestImage(img, null);
        
        // Unlike most Ops, this one creates a clone of the original image
        
        // Except there's a strange exception for TYPE_USHORT_GRAY ???
        if (type == BufferedImage.TYPE_USHORT_GRAY)
          {
            harness.check(dest.getColorModel().getPixelSize(), 8);
            harness.check(dest.getColorModel().getTransferType(), DataBuffer.TYPE_BYTE);
          }
        else
          {
            harness.check(dest.getColorModel().getPixelSize(),
                          img.getColorModel().getPixelSize());
            harness.check(dest.getColorModel().getTransferType(), img.getColorModel().getTransferType());
          }

        harness.check(dest.getColorModel().getClass() == img.getColorModel().getClass());
        harness.check(dest.getSampleModel().getClass() == img.getSampleModel().getClass());
        harness.check(dest.getColorModel().getNumComponents(), img.getColorModel().getNumComponents());
        harness.check(dest.getColorModel().getTransparency(), img.getColorModel().getTransparency());
        harness.check(dest.getColorModel().hasAlpha(), img.getColorModel().hasAlpha());
        harness.check(dest.getColorModel().isAlphaPremultiplied(), img.getColorModel().isAlphaPremultiplied());
        harness.check(dest.getColorModel().getColorSpace().getType(), img.getColorModel().getColorSpace().getType());
        harness.check(dest.getRaster().getNumBands(), img.getRaster().getNumBands());
        harness.check(dest.getRaster().getNumDataElements(), img.getRaster().getNumDataElements());
      }
  }
}
 
