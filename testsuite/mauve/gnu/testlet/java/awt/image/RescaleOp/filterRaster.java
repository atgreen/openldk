/* filterRaster.java -- some checks for the filter() methods in the
                        RescaleOp class.
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

package gnu.testlet.java.awt.image.RescaleOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.BufferedImage;
import java.awt.image.RescaleOp;
import java.awt.image.WritableRaster;

public class filterRaster implements Testlet
{
  public void test(TestHarness harness)
  {
    simpleTests(harness);
    testRaster1(harness);
    testRaster2(harness);
    testMismatch(harness);
  }
  
  private void simpleTests(TestHarness harness)
  {
    harness.checkPoint("filter(Raster)");
    
    // Create an image to work on
    BufferedImage img = new BufferedImage(20, 20, BufferedImage.TYPE_USHORT_GRAY);
    WritableRaster r = img.getRaster();
    r.setSample(1, 1, 0, 150);
    
    RescaleOp op = new RescaleOp(1, 1, null);
    
    // Src and dst rasters can be the same
    try
    {
      op.filter(r, r);
      harness.check(true);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    // Src and dst are different sizes (not allowed, unlike some other Ops)
    BufferedImage dst = new BufferedImage(30, 40, BufferedImage.TYPE_INT_RGB);
    try
    {
      op.filter(r, dst.getRaster());
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    // Null destination check
    WritableRaster dstRast = op.filter(r, null);
    harness.check(dstRast.getHeight(), r.getHeight());
    harness.check(dstRast.getWidth(), r.getWidth());
    harness.check(dstRast.getMinX(), r.getMinX());
    harness.check(dstRast.getMinY(), r.getMinY());
    harness.check(dstRast.getNumBands(), r.getNumBands());
    harness.check(dstRast.getNumDataElements(), r.getNumDataElements());
    harness.check(dstRast.getTransferType(), r.getTransferType());
    harness.check(dstRast.getBounds(), r.getBounds());
    harness.check(dstRast.getDataBuffer().getClass(), r.getDataBuffer().getClass());
    
    // Test positive & negative clipping behaviour
    img.getRaster().setSample(1, 1, 0, 1500);
    op = new RescaleOp(100, 0, null);
    dstRast = op.filter(r, null);
    double maxValue = Math.pow(2, r.getSampleModel().getSampleSize(0)) - 1;
    harness.check(dstRast.getSample(1, 1, 0), maxValue);
    
    op = new RescaleOp(1, -2000, null);
    dstRast = op.filter(r, null);
    harness.check(dstRast.getSample(1, 1, 0), 0);
  }
  
  public void testRaster1(TestHarness harness)
  {
    harness.checkPoint("filter(Raster) with one scaling factor");

    // Create a raster to work on
    BufferedImage img = new BufferedImage(20, 20, BufferedImage.TYPE_INT_ARGB);
    WritableRaster r = img.getRaster();
    r.setSample(1, 1, 0, 150);
    r.setSample(1, 1, 1, 160);
    r.setSample(1, 1, 2, 175);
    r.setSample(1, 1, 3, 195);
    
    r.setSample(1, 3, 0, 45);
    r.setSample(1, 3, 1, 60);
    r.setSample(1, 3, 2, 70);
    r.setSample(1, 3, 3, 90);
    
    RescaleOp op = new RescaleOp(0.75f, 25f, null);
    WritableRaster dest = op.filter(r, null);
    
    harness.check(dest.getSample(1, 1, 0), 137);    //rounded down from 137.5
    harness.check(dest.getSample(1, 1, 1), 145);
    harness.check(dest.getSample(1, 1, 3), 171);    //rounded down from 171.25
    harness.check(dest.getSample(1, 1, 2), 156);    //rounded down from 156.25 
    
    harness.check(dest.getSample(1, 3, 0), 58);     //rounded down from 58.75
    harness.check(dest.getSample(1, 3, 1), 70);
    harness.check(dest.getSample(1, 3, 2), 77);     //rounded down from 77.5
    harness.check(dest.getSample(1, 3, 3), 92);     //rounded down from 92.5 
  }
    
  public void testRaster2(TestHarness harness)
  {
    harness.checkPoint("filter(Raster) with multiple factors");

    // Create an image to work on
    BufferedImage img = new BufferedImage(20, 20, BufferedImage.TYPE_INT_ARGB);
    WritableRaster r = img.getRaster();
    r.setSample(1, 1, 0, 10);
    r.setSample(1, 1, 1, 20);
    r.setSample(1, 1, 2, 35);
    r.setSample(1, 1, 3, 40);
    
    r.setSample(1, 3, 0, 45);
    r.setSample(1, 3, 1, 60);
    r.setSample(1, 3, 2, 70);
    r.setSample(1, 3, 3, 90);
    
    RescaleOp op = new RescaleOp(new float[]{0.75f, 2.5f, -1f, 0f},
                             new float[]{25f, 2f, 1f, 0f},
                             null);
    WritableRaster dest = op.filter(r, null);
    
    harness.check(dest.getSample(1, 1, 0), 32);     //rounded down from 32.5 
    harness.check(dest.getSample(1, 1, 1), 52);
    harness.check(dest.getSample(1, 1, 2), 0);
    harness.check(dest.getSample(1, 1, 3), 0);
    
    harness.check(dest.getSample(1, 3, 0), 58);     //rounded down from 58.75
    harness.check(dest.getSample(1, 3, 1), 152);
    harness.check(dest.getSample(1, 3, 2), 0);
    harness.check(dest.getSample(1, 3, 3), 0);
  }
  
  private void testMismatch(TestHarness harness)
  {
    harness.checkPoint("filter(Raster) with mismatched arrays");

    // Create an image to work on
    BufferedImage img = new BufferedImage(20, 20, BufferedImage.TYPE_INT_ARGB);
    WritableRaster r = img.getRaster();
    r.setSample(1, 1, 0, 10);
    r.setSample(1, 1, 1, 20);
    r.setSample(1, 1, 2, 35);
    r.setSample(1, 1, 3, 40);
    
    r.setSample(1, 3, 0, 45);
    r.setSample(1, 3, 1, 60);
    r.setSample(1, 3, 2, 70);
    r.setSample(1, 3, 3, 90);

    // Test mismatched arrays
    RescaleOp op = new RescaleOp(new float[]{1, 2, 3, 4}, new float[]{1, 2, 3}, null);
    try
    {
      op.filter(r, null);
      harness.check(false);
    }
    catch (IllegalArgumentException ex)
    {
      harness.check(true);
    }
    
    // Only the first value from both arrays is read if the offsets array
    // has only one value
    op = new RescaleOp(new float[]{1, 2, 3, 4}, new float[]{1}, null);
    try
    {
      WritableRaster dest = op.filter(r, null);
      harness.check(dest.getSample(1, 1, 0), 11);
      harness.check(dest.getSample(1, 1, 1), 21);
      harness.check(dest.getSample(1, 3, 0), 46);
      harness.check(dest.getSample(1, 3, 1), 61);
    }
    catch (IllegalArgumentException ex)
    {
      harness.check(false);
    }
    
    // Same with a single-length factors array
    op = new RescaleOp(new float[]{0.5f}, new float[]{2, 3, 4, 5}, null);
    try
    {
      WritableRaster dest = op.filter(r, null);
      harness.check(dest.getSample(1, 1, 0), 7);
      harness.check(dest.getSample(1, 1, 1), 12);
      harness.check(dest.getSample(1, 3, 0), 24);
      harness.check(dest.getSample(1, 3, 1), 32);
    }
    catch (IllegalArgumentException ex)
    {
      harness.check(false);
    }
  }
}
