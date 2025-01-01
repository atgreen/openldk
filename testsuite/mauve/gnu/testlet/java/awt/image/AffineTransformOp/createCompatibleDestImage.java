/* createCompatibleDestImage.java -- some checks for the
       createCompatibleDestImage() method of the AffineTransformOp class.
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

package gnu.testlet.java.awt.image.AffineTransformOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.color.ColorSpace;
import java.awt.geom.AffineTransform;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.ComponentColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DirectColorModel;
import java.awt.image.PixelInterleavedSampleModel;
import java.awt.image.SinglePixelPackedSampleModel;

/**
 * Checks for the createCompatibleDestImage method in the
 * {@link AffineTransformOp} class.
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
    AffineTransform xform = new AffineTransform();
    AffineTransformOp op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    
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
    AffineTransform xform = new AffineTransform();
    AffineTransformOp op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);

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
        
        dest = op.createCompatibleDestImage(img, null);
        harness.check(dest.getColorModel().isCompatibleSampleModel(dest.getSampleModel()));

        // This ensures that we have the same defaults as the reference implementation
        switch (type)
        {
          case BufferedImage.TYPE_INT_RGB:
          case BufferedImage.TYPE_INT_ARGB:
          case BufferedImage.TYPE_INT_ARGB_PRE:
          case BufferedImage.TYPE_3BYTE_BGR:
          case BufferedImage.TYPE_USHORT_565_RGB:
          case BufferedImage.TYPE_USHORT_555_RGB:
          case BufferedImage.TYPE_BYTE_GRAY:
          case BufferedImage.TYPE_USHORT_GRAY:
            
            if (type == BufferedImage.TYPE_INT_ARGB_PRE)
              harness.check(dest.getType(), BufferedImage.TYPE_INT_ARGB_PRE);
            else
              harness.check(dest.getType(), BufferedImage.TYPE_INT_ARGB);

            harness.check(dest.getColorModel() instanceof DirectColorModel);
            harness.check(dest.getSampleModel() instanceof SinglePixelPackedSampleModel);
            harness.check(dest.getColorModel().getPixelSize(), 32);
            harness.check(dest.getColorModel().getNumComponents(), 4);
            harness.check(dest.getColorModel().getTransparency(), ColorModel.TRANSLUCENT);
            harness.check(dest.getColorModel().hasAlpha(), true);
            harness.check(dest.getColorModel().getTransferType(), DataBuffer.TYPE_INT);
            harness.check(dest.getColorModel().isAlphaPremultiplied(), (type == BufferedImage.TYPE_INT_ARGB_PRE));
            harness.check(dest.getColorModel().getColorSpace().getType(), ColorSpace.TYPE_RGB);
            harness.check(dest.getRaster().getNumBands(), 4);
            harness.check(dest.getRaster().getNumDataElements(), 1);
            break;

          case BufferedImage.TYPE_4BYTE_ABGR:
          case BufferedImage.TYPE_4BYTE_ABGR_PRE:
            harness.check(dest.getColorModel() instanceof ComponentColorModel);
            harness.check(dest.getSampleModel() instanceof PixelInterleavedSampleModel);
            harness.check(dest.getColorModel().getPixelSize(), 32);
            harness.check(dest.getColorModel().getNumComponents(), 4);
            harness.check(dest.getColorModel().getTransparency(),  ColorModel.TRANSLUCENT);
            harness.check(dest.getColorModel().hasAlpha(), true);
            harness.check(dest.getColorModel().getTransferType(), DataBuffer.TYPE_BYTE);
            harness.check(dest.getColorModel().isAlphaPremultiplied(), (type == BufferedImage.TYPE_4BYTE_ABGR_PRE));
            harness.check(dest.getColorModel().getColorSpace().getType(), ColorSpace.TYPE_RGB);
            harness.check(dest.getRaster().getNumBands(), 4);
            harness.check(dest.getRaster().getNumDataElements(), 4);
            break;
        }
      }
  }
}


 