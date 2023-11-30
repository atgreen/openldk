/* createCompatibleDestRaster.java -- some checks for the
              createCompatibleDestRaster() method of the ColorConvertOp class.
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

package gnu.testlet.java.awt.image.ColorConvertOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Point;
import java.awt.color.ColorSpace;
import java.awt.color.ICC_Profile;
import java.awt.image.ColorConvertOp;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.PixelInterleavedSampleModel;
import java.awt.image.Raster;
import java.util.Arrays;

/**
 * Checks for the createCompatibleDestRaster method in the
 * {@link ColorConvertOp} class.
 */
public class createCompatibleDestRaster implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("createCompatibleDestRaster");

    simpleTest(harness);

    // Try with all possible colorspaces
    int[] models = new int[] {ColorSpace.CS_sRGB,
                              ColorSpace.CS_CIEXYZ,
                              ColorSpace.CS_GRAY,
                              ColorSpace.CS_LINEAR_RGB,
                              ColorSpace.CS_PYCC};
    
    for (int i = 0; i < models.length; i++)
      for (int j = 0; j < models.length; j++)
        colorModelTest(harness, models[i], models[j]);
    
    // Specify profile list
    profileTest(harness, new ICC_Profile[] {ICC_Profile.getInstance(ColorSpace.CS_LINEAR_RGB),
                                            ICC_Profile.getInstance(ColorSpace.CS_CIEXYZ),
                                            ICC_Profile.getInstance(ColorSpace.CS_sRGB),
                                            ICC_Profile.getInstance(ColorSpace.CS_GRAY)});
                             
    profileTest(harness, new ICC_Profile[] {ICC_Profile.getInstance(ColorSpace.CS_GRAY),
                                            ICC_Profile.getInstance(ColorSpace.CS_sRGB)});
                             
    profileTest(harness, new ICC_Profile[] {ICC_Profile.getInstance(ColorSpace.CS_GRAY),
                                            ICC_Profile.getInstance(ColorSpace.CS_CIEXYZ)});
  }
  
  private void simpleTest(TestHarness harness)
  {
    // This method can never be used with these constructors
    ColorConvertOp op = new ColorConvertOp(null);
    Raster src = Raster.createBandedRaster(DataBuffer.TYPE_BYTE, 10, 10,
                                           3, new Point(5, 5));
    try
    {
      op.createCompatibleDestRaster(src);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    ColorSpace cs = ColorSpace.getInstance(ColorSpace.CS_sRGB);
    op = new ColorConvertOp(cs, null);
    try
    {
      op.createCompatibleDestRaster(src);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
  }
  
  private void colorModelTest(TestHarness harness, int cspace1, int cspace2)
  {
    harness.checkPoint("two colorspaces defined, " + cspace1     + ", " + cspace2);

    ColorSpace cs = ColorSpace.getInstance(cspace1);
    ColorSpace cs2 = ColorSpace.getInstance(cspace2);
    ColorConvertOp op = new ColorConvertOp(cs, cs2, null);
    int bands = cs2.getNumComponents();

    Raster src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, bands, new Point(5, 5));

    try
    {
      Raster dst = op.createCompatibleDestRaster(src);
      harness.check(dst.getHeight(), src.getHeight());
      harness.check(dst.getWidth(), src.getWidth());
      harness.check(dst.getNumBands(), bands);
      harness.check(dst.getTransferType(), DataBuffer.TYPE_BYTE);
      harness.check(dst.getDataBuffer().getDataType(), DataBuffer.TYPE_BYTE);
      harness.check(dst.getNumDataElements(), cs2.getNumComponents());
      harness.check(dst.getSampleModel() instanceof PixelInterleavedSampleModel);
      harness.check(dst.getDataBuffer() instanceof DataBufferByte);
      
      PixelInterleavedSampleModel sm = (PixelInterleavedSampleModel)dst.getSampleModel();

      harness.check(sm.getPixelStride(), cs2.getNumComponents());
      harness.check(sm.getScanlineStride(), cs2.getNumComponents() * src.getWidth());
      int[] expected = new int[cs2.getNumComponents()];
      
      for (int i = 0; i < expected.length; i++)
        expected[i] = i;
      harness.check(Arrays.equals(sm.getBandOffsets(), expected));
      
      harness.check(dst.getDataBuffer().getNumBanks(), 1);
      harness.check(dst.getDataBuffer().getOffset(), 0);
      harness.check(dst.getDataBuffer().getSize(), src.getHeight() * src.getWidth() * cs2.getNumComponents());
      
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    // Try a different type
    src = Raster.createBandedRaster(DataBuffer.TYPE_USHORT, 25, 40, bands, new Point(5, 5));
    try
    {
      Raster dst = op.createCompatibleDestRaster(src);
      harness.check(dst.getNumBands(), bands);
      harness.check(dst.getTransferType(), DataBuffer.TYPE_BYTE);
      harness.check(dst.getDataBuffer().getDataType(), DataBuffer.TYPE_BYTE);
      harness.check(dst.getNumDataElements(), cs2.getNumComponents());
      harness.check(dst.getSampleModel() instanceof PixelInterleavedSampleModel);
      harness.check(dst.getDataBuffer() instanceof DataBufferByte);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    // Try different number of bands in the source; the destination should
    // ignore this and always have ColorSpace.getNumComponents() bands
    for (int i = 1; i < bands + 5; i++)
      {
        src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, i, new Point(5, 5));
        try
        {
          Raster dst = op.createCompatibleDestRaster(src);
          harness.check(dst.getNumBands(), cs2.getNumComponents());
          harness.check(dst.getNumDataElements(), cs2.getNumComponents());
        }
        catch (IllegalArgumentException e)
        {
          harness.check(false);
        }
      }
  }
  
  private void profileTest(TestHarness harness, ICC_Profile[] profiles)
  {
    harness.checkPoint("profile test, " + profiles[profiles.length-1].getClass().getName());

    ColorConvertOp op = new ColorConvertOp(profiles, null);
    
    Raster src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, 5, new Point(5, 5));

    try
    {
      Raster dst = op.createCompatibleDestRaster(src);
      harness.check(dst.getHeight(), src.getHeight());
      harness.check(dst.getWidth(), src.getWidth());
      
      // It appears we always use TYPE_BYTE regardless of the source raster
      harness.check(dst.getTransferType(), DataBuffer.TYPE_BYTE);
      harness.check(dst.getDataBuffer().getDataType(), DataBuffer.TYPE_BYTE);
      
      // GRAY is the exception with 1 band; all others have 3
      if (profiles[profiles.length-1].getColorSpaceType() == ColorSpace.TYPE_GRAY)
        {
          harness.check(dst.getNumBands(), 1);
          harness.check(dst.getNumDataElements(), 1);
        }
      else
        {
          harness.check(dst.getNumBands(), 3);
          harness.check(dst.getNumDataElements(), 3);
        }
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    // Try different numbers of bands in the source; the destination will
    // ignore this and always have ColorSpace.getNumComponents() bands
    
    // Essentially the dest raster will be identical to the case above, 
    // regardless of number of source bands (which makes sense)
    for (int i = 1; i < 5; i++)
      {
        src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, i, new Point(5, 5));
        try
        {
          Raster dst = op.createCompatibleDestRaster(src);
          harness.check(dst.getTransferType(), DataBuffer.TYPE_BYTE);
          harness.check(dst.getDataBuffer().getDataType(), DataBuffer.TYPE_BYTE);
          
          if (profiles[profiles.length-1].getColorSpaceType() == ColorSpace.TYPE_GRAY)
            {
              harness.check(dst.getNumBands(), 1);
              harness.check(dst.getNumDataElements(), 1);
            }
          else
            {
              harness.check(dst.getNumBands(), 3);
              harness.check(dst.getNumDataElements(), 3);
            }
        }
        catch (IllegalArgumentException e)
        {
          harness.check(false);
        }
      }
  }
}

