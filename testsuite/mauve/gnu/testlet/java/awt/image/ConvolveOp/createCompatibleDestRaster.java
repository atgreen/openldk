/* createCompatibleDestRaster.java -- some checks for the
              createCompatibleDestRaster() method of the ConvolveOp class.
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

package gnu.testlet.java.awt.image.ConvolveOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Point;
import java.awt.image.ConvolveOp;
import java.awt.image.DataBuffer;
import java.awt.image.Kernel;
import java.awt.image.Raster;

/**
 * Checks for the createCompatibleDestRaster method in the
 * {@link ConvolveOp} class.
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

    // Simple test
    Raster src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, 3, new Point(5, 5));
    Kernel kern = new Kernel(3, 3, new float[] {1f, 2f, 3f, 4f, 5f, 6f, 7f, 8f, 9f});
    ConvolveOp op = new ConvolveOp(kern, ConvolveOp.EDGE_NO_OP, null);
    
    try
    {
      Raster dst = op.createCompatibleDestRaster(src);
      harness.check(dst.getHeight(), src.getHeight());
      harness.check(dst.getWidth(), src.getWidth());
      harness.check(dst.getNumBands(), src.getNumBands());
      harness.check(dst.getTransferType(), src.getTransferType());
      harness.check(dst.getDataBuffer().getDataType(), src.getDataBuffer().getDataType());
      harness.check(dst.getNumDataElements(), src.getNumDataElements());
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    // Try a different type
    src = Raster.createBandedRaster(DataBuffer.TYPE_BYTE, 25, 40, 3, new Point(5, 5));
    try
    {
      Raster dst = op.createCompatibleDestRaster(src);
      harness.check(dst.getNumBands(), src.getNumBands());
      harness.check(dst.getTransferType(), src.getTransferType());
      harness.check(dst.getDataBuffer().getDataType(), src.getDataBuffer().getDataType());
      harness.check(dst.getNumDataElements(), src.getNumDataElements());
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    // Try a different number of bands
    src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, 5, new Point(5, 5));
    try
    {
      Raster dst = op.createCompatibleDestRaster(src);
      harness.check(dst.getNumBands(), src.getNumBands());
      harness.check(dst.getTransferType(), src.getTransferType());
      harness.check(dst.getDataBuffer().getDataType(), src.getDataBuffer().getDataType());
      harness.check(dst.getNumDataElements(), src.getNumDataElements());
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
  }
}

