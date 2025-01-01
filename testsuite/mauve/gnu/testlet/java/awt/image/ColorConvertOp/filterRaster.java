/* filterRaster.java -- some checks for the filter(Raster) method of the
              ColorConvertOp class.
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

import java.awt.Graphics2D;
import java.awt.color.ColorSpace;
import java.awt.geom.Line2D;
import java.awt.image.BufferedImage;
import java.awt.image.ColorConvertOp;
import java.awt.image.WritableRaster;

/**
 * Checks the filter(Raster) method in the {@link ColorConvertOp} class.
 */
public class filterRaster implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("filter(Raster)");
    
    // Create a raster to work on
    BufferedImage img = new BufferedImage(20, 20, BufferedImage.TYPE_INT_RGB);
    Graphics2D g = (Graphics2D)img.getGraphics();
    g.draw(new Line2D.Double(0, 0, 20, 20));
    
    ColorSpace cs1 = ColorSpace.getInstance(ColorSpace.CS_sRGB);
    ColorSpace cs2 = ColorSpace.getInstance(ColorSpace.CS_GRAY);
    ColorConvertOp op = new ColorConvertOp(cs1, cs2, null);
    WritableRaster raster = img.getRaster();
    
    // Src and dst rasters cannot be the same (different from 
    // filter(BufferedImage, BufferedImage) )
    try
    {
      op.filter(raster, raster);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    // Src and dst are different sizes (not allowed, unlike some other Ops)
    BufferedImage dst = new BufferedImage(30, 40, BufferedImage.TYPE_BYTE_GRAY);
    WritableRaster raster2 = dst.getRaster();
    try
    {
      op.filter(raster, raster2);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    // Null destination
    WritableRaster dstRaster = op.filter(raster, null);
    harness.check(dstRaster.getTransferType(), op.createCompatibleDestRaster(raster).getTransferType());
    harness.check(dstRaster.getNumBands(), op.createCompatibleDestRaster(raster).getNumBands());
    harness.check(dstRaster.getNumDataElements(), op.createCompatibleDestRaster(raster).getNumDataElements());

    // Incompatible constructor (ie, not enough information)
    op = new ColorConvertOp(null);
    try
    {
      op.filter(raster, null);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    op = new ColorConvertOp(cs1, null);
    try
    {
      op.filter(raster, null);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    // Destination raster incompatible with defined conversion;
    // ie, with this conversion, cs2 is TYPE_GRAY thus a dest raster of
    // TYPE_RGB will have the wrong number of data elements
    op = new ColorConvertOp(cs1, cs2, null);
    dstRaster = new BufferedImage(20, 20, BufferedImage.TYPE_INT_ARGB).getRaster();
    
    try
    {
      op.filter(raster, dstRaster);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    // Also true if source raster is incompatible
    raster = new BufferedImage(20, 20, BufferedImage.TYPE_BYTE_GRAY).getRaster();
    try
    {
      op.filter(raster, raster2);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
  }
}
