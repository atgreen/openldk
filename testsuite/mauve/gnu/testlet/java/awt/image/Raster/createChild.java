/* createChild.java -- some checks for the createChild() method
       in the Raster class.
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

// Tags: JDK1.4
// Uses: MyRaster

package gnu.testlet.java.awt.image.Raster;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Point;
import java.awt.image.DataBuffer;
import java.awt.image.Raster;
import java.awt.image.RasterFormatException;
import java.awt.image.SinglePixelPackedSampleModel;
import java.awt.image.WritableRaster;

public class createChild implements Testlet 
{
  public void test(TestHarness harness)
  {
    testData(harness);
    testBounds(harness);
    testBands(harness);
  }
  
  private void testData(TestHarness harness)
  {
    Raster rst = createRaster(harness);
    
    // Child raster
    Raster rst2 = rst.createChild(10, 20, 25, 15, 0, 0, null);
    harness.check(!(rst2 instanceof WritableRaster));
    harness.check(rst2.getMinX(), 0);
    harness.check(rst2.getMinY(), 0);
    harness.check(rst2.getWidth(), 25);
    harness.check(rst2.getHeight(), 15);
    
    for (int x = 0; x < 25; x++)
      for (int y = 0; y < 15; y++)
        for (int b = 0; b < 3; b++)
          harness.check(rst2.getSample(x, y, b), x+10 + y+20 + b);
    
    // Offset child
    rst2 = rst.createChild(10, 20, 25, 15, 30, 40, null);
    harness.check(rst2.getMinX(), 30);
    harness.check(rst2.getMinY(), 40);
    harness.check(rst2.getWidth(), 25);
    harness.check(rst2.getHeight(), 15);
    
    for (int x = 30; x < 55; x++)
      for (int y = 40; y < 55; y++)
        for (int b = 0; b < 3; b++)
          harness.check(rst2.getSample(x, y, b), x-20 + y-20 + b);
  }
  
  private void testBounds(TestHarness harness)
  {
    Raster rst = createRaster(harness);

    // Width and height out of bounds
    try
    {
      rst.createChild(10, 20, 100, 100, 0, 0, null);
      harness.check(false);
    }
    catch (RasterFormatException ex)
    {
      harness.check(true);
    }
    catch (Exception ex)
    {
      harness.check(false);
    }
    
    // MinX and MinY out of bounds
    try
    {
      // Create child with non-zero minX and minY
      Raster rst2 = rst.createChild(0, 0, 25, 25, 30, 30, null);
      
      // Create child's child with minX and minY out of bounds
      rst2.createChild(10, 20, 10, 10, 0, 0, null);
      harness.check(false);
    }
    catch (RasterFormatException ex)
    {
      harness.check(true);
    }
    catch (Exception ex)
    {
      harness.check(false);
    }
  }
  
  private void testBands(TestHarness harness)
  {
    Raster rst = createRaster(harness);
    
    // Copy all bands
    Raster rst2 = rst.createChild(0, 0, 50, 40, 0, 0, null);
    harness.check(rst2.getNumBands(), rst.getNumBands());
    
    // Only first two bands
    rst2 = rst.createChild(0, 0, 50, 40, 0, 0, new int[]{0, 1});
    harness.check(rst2.getNumBands(), 2);
    for (int x = 0; x < 50; x++)
      for (int y = 0; y < 40; y++)
        for (int b = 0; b < 2; b++)
          harness.check(rst2.getSample(x, y, b), x+y+b);
    
    // Only last two bands
    rst2 = rst.createChild(0, 0, 50, 40, 0, 0, new int[]{1, 2});
    harness.check(rst2.getNumBands(), 2);
    for (int x = 0; x < 50; x++)
      for (int y = 0; y < 40; y++)
        for (int b = 0; b < 2; b++)
          harness.check(rst2.getSample(x, y, b), x+y+b+1);

    // Only middle band
    rst2 = rst.createChild(0, 0, 50, 40, 0, 0, new int[]{1});
    harness.check(rst2.getNumBands(), 1);
    for (int x = 0; x < 50; x++)
      for (int y = 0; y < 40; y++)
        harness.check(rst2.getSample(x, y, 0), x+y+1);
    
    // Reverse order of bands
    rst2 = rst.createChild(0, 0, 50, 40, 0, 0, new int[]{2, 0});
    harness.check(rst2.getNumBands(), 2);
    for (int x = 0; x < 50; x++)
      for (int y = 0; y < 40; y++)
        {        
          harness.check(rst2.getSample(x, y, 0), x+y+2);
          harness.check(rst2.getSample(x, y, 1), x+y);
        }
  }
  
  private Raster createRaster(TestHarness harness)
  {
    // Create initial raster
    WritableRaster rst = Raster.createWritableRaster(new SinglePixelPackedSampleModel(DataBuffer.TYPE_INT,
                                                                                      50, 40,
                                                                                      new int[]{0xff0000, 0xff00, 0xff}),
                                                     new Point(0, 0));

    // Fill with test data
    for (int x = 0; x < 50; x++)
      for (int y = 0; y < 40; y++)
        for (int b = 0; b < 3; b++)
          rst.setSample(x, y, b, x+y+b);
    
    // Get non-writable version with the same data
    Raster rst2 = new MyRaster(rst.getSampleModel(),
                               rst.getDataBuffer(),
                               new Point(0, 0));
    harness.check(!(rst2 instanceof WritableRaster));
    return rst2;
  }
}
