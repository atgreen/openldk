/* filterRaster.java -- some checks for the filter(Raster) method of the
              AffineTransformOp class.
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

import java.awt.Graphics2D;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.awt.image.ImagingOpException;
import java.awt.image.WritableRaster;

/**
 * Checks the filter(BufferedImage) method in the {@link AffineTransformOp} class.
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
    testDefaults(harness, AffineTransformOp.TYPE_BILINEAR);
    testDefaults(harness, AffineTransformOp.TYPE_BICUBIC);
    testDefaults(harness, AffineTransformOp.TYPE_NEAREST_NEIGHBOR);
    
    // Should write a test to ensure that the filter can actually be applied
    // properly.  Since interpolation comes into play, however, I don't know
    // of a good way to test it, as our filtered image will be different
    // from the reference implementation's...
  }
  
  private void testDefaults(TestHarness harness, int type)
  {
    // Create a raster to work on based on an image
    BufferedImage img = new BufferedImage(20, 20, BufferedImage.TYPE_USHORT_GRAY);
    Graphics2D g = (Graphics2D)img.getGraphics();
    g.draw(new Line2D.Double(0, 0, 20, 20));
    
    WritableRaster src = img.getRaster();
    AffineTransform xform = new AffineTransform();
    AffineTransformOp op = new AffineTransformOp(xform, type);
    
    // Make sure it works in the first place
    WritableRaster dest = src.createCompatibleWritableRaster();
    try
    {
      op.filter(src, dest);
      harness.check(true);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    // Src and dst cannot be the same
    try
    {
      op.filter(src, src);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    /*
     * This is not allowed on the ref impl, but it can't hurt to allow it
     * here...
     * 
    // Different type of raster (not allowed)
    dest = Raster.createBandedRaster(DataBuffer.TYPE_USHORT, 20, 20, 1, new Point(0,0));
    try
    {
      op.filter(src, dest);
      harness.check(false);
    }
    catch (ImagingOpException e)
    {
      harness.check(true);
    }
     */
    
    // Different size (allowed)
    dest = src.createCompatibleWritableRaster(75, 87);
    try
    {
      op.filter(src, dest);
      harness.check(true);
    }
    catch (ImagingOpException e)
    {
      harness.check(false);
    }
    
    // Null destination (allowed - will create one for us)
    WritableRaster dest2 = op.filter(src, null);
    harness.check(dest2 != null);
  }
}

