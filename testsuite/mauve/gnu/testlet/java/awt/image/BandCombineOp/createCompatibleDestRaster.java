/* createCompatibleDestRaster.java -- some checks for the
              createCompatibleDestRaster() method of the BandCombineOp class.
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

package gnu.testlet.java.awt.image.BandCombineOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Point;
import java.awt.image.BandCombineOp;
import java.awt.image.DataBuffer;
import java.awt.image.Raster;

/**
 * Checks for the createCompatibleDestRaster method in the
 * {@link BandCombineOp} class.
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
    basicTest(harness);
    test2(harness);
    test3(harness);
    impossibleTest(harness);
  }
  
  private void basicTest(TestHarness harness)
  {
    harness.checkPoint("createCompatibleDestRaster");

    // Simple test
    float[][] matrix = new float[][] {new float[] {1, 2, 3},
                                      new float[] {4, 5, 6},
                                      new float[] {7, 8, 9}};
    
    Raster src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, 3, new Point(5, 5));
    BandCombineOp op = new BandCombineOp(matrix, null);
    
    try
    {
      Raster dst = op.createCompatibleDestRaster(src);
      harness.check(dst.getNumBands(), 3);
      harness.check(dst.getHeight(), src.getHeight());
      harness.check(dst.getWidth(), src.getWidth());
      harness.check(dst.getTransferType(), src.getTransferType());
      harness.check(dst.getDataBuffer().getDataType(), src.getDataBuffer().getDataType());
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
      harness.check(dst.getTransferType(), src.getTransferType());
      harness.check(dst.getDataBuffer().getDataType(), src.getDataBuffer().getDataType());
      harness.check(dst.getNumBands(), 3);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    // This is where things get messy.  The Sun API states that "The width of the matrix
    // must be equal to the number of bands in the source Raster, optionally plus one. If
    // there is one more column in the matrix than the number of bands, there is an implied
    // 1 at the end of the vector of band samples representing a pixel. The height of the matrix 
    // must be equal to the number of bands in the destination", but this is NOT how their
    // implementation behaves.
    //
    // They miss one requirement, however: the number of bands in the source and destination
    // rasters must also be equal... which effectively means
    // ((width == height) || (width == height + 1)) must be true
    
    // The source (4 bands) is incompatible with the matrix (width = 3)

    src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, 4, new Point(5, 5));
    try
    {
      op.createCompatibleDestRaster(src);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    // The destination raster (3 bands) would be incompatibel with the source (2 bands)
    // (the undocumented requirement)
    src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, 1, new Point(5, 5));
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

  // Using a non-square matrix
  private void test2(TestHarness harness)
  {
    // Height still == 3, but width == 4
    float[][] matrix = new float[][] {new float[] {1, 2, 3, 1},
                                      new float[] {4, 5, 6, 1},
                                      new float[] {7, 8, 9, 1}};

    BandCombineOp op = new BandCombineOp(matrix, null);

    // Source has 3 bands, which is the width minus one (uses the implied 1), works
    Raster src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, 3, new Point(5, 5));
    try
    {
      Raster dst = op.createCompatibleDestRaster(src);
      harness.check(dst.getNumBands(), 3);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }

    // Source has 4 bands, which is compatible with the matrix (width is 4)
    // The destination raster, however, is not compatible with the source (3 vs 4 bands)
    // (the undocumented restriction)
    src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, 4, new Point(5, 5));
    try
    {
      op.createCompatibleDestRaster(src);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }

    // Also incompatible, but this is expected according to the spec
    src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, 2, new Point(5, 5));
    try
    {
      op.createCompatibleDestRaster(src);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }

    // Also still incompatible
    src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, 5, new Point(5, 5));
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

  // One more test, with a larger matrix
  private void test3(TestHarness harness)
  {
    // Test again, with a different matrix
    float[][] matrix = new float[][] {new float[] {1, 2, 3, 1, 5},
                                      new float[] {4, 5, 6, 1, 5},
                                      new float[] {7, 8, 9, 1, 5},
                                      new float[] {1, 2, 3, 4, 5}};

    BandCombineOp op = new BandCombineOp(matrix, null);

    // Works using the implied 1
    Raster src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, 4, new Point(5, 5));
    try
    {
      Raster dst = op.createCompatibleDestRaster(src);
      harness.check(dst.getNumBands(), 4);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }

    // Does not use implied 1; however source and dest would have incompabible bands
    // (undocumented restriction)
    src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, 5, new Point(5, 5));
    try
    {
      op.createCompatibleDestRaster(src);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }

    // Just for completeness
    src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, 3, new Point(5, 5));
    try
    {
      op.createCompatibleDestRaster(src);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, 6, new Point(5, 5));
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
  
  private void impossibleTest(TestHarness harness)
  {
    // Thus, it would be impossible to create a compatible destination wraster if
    // ((width != height) && (width != height + 1))
    float[][] matrix = new float[][] {new float[] {1, 2, 3, 1, 5},
                                      new float[] {4, 5, 6, 1, 5},
                                      new float[] {7, 8, 9, 1, 5}};

    BandCombineOp op = new BandCombineOp(matrix, null);

    for (int i = 2; i < 6; i++)
      {
        Raster src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, i, new Point(5, 5));
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

    // Repeat the above test, but with too many rows instead of too few
    matrix = new float[][] {new float[] {1, 2, 3,},
                            new float[] {4, 5, 6,},
                            new float[] {2, 4, 6,},
                            new float[] {1, 3, 5,},
                            new float[] {7, 8, 9,}};

    op = new BandCombineOp(matrix, null);

    for (int i = 2; i < 6; i++)
      {
        Raster src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 25, 40, i, new Point(5, 5));
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
  }
}

