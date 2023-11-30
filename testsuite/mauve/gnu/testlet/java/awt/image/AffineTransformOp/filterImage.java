/* filterImage.java -- some checks for the filter(Image) method of the
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

/**
 * Checks the filter(BufferedImage) method in the {@link AffineTransformOp} class.
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
    
    // Should write a test to ensure that the filter can actually be applied
    // properly.  Since interpolation comes into play, however, I don't know
    // of a good way to test it, as our filtered image will be different
    // from the reference implementation's...
  }
  
  private void testDefaults(TestHarness harness)
  {
    // Create an image to work on
    BufferedImage img = new BufferedImage(20, 20, BufferedImage.TYPE_USHORT_GRAY);
    Graphics2D g = (Graphics2D)img.getGraphics();
    g.draw(new Line2D.Double(0, 0, 20, 20));
    
    AffineTransform xform = new AffineTransform();
    AffineTransformOp op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    
    // Src and dst images cannot be the same
    try
    {
      op.filter(img, img);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    // Src and dst are different sizes (allowed)
    BufferedImage dst = new BufferedImage(30, 40, BufferedImage.TYPE_USHORT_GRAY);
    try
    {
      op.filter(img, dst);
      harness.check(true);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    // Src and dst have different tpyes (allowed)
    dst = new BufferedImage(20, 20, BufferedImage.TYPE_INT_ARGB);
    try
    {
      op.filter(img, dst);
      harness.check(true);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    // Src and dst are different sizes AND different types (not allowed)
    /*
     * Fails on the ref impl...
     *
    dst = new BufferedImage(30, 40, BufferedImage.TYPE_INT_ARGB);
    try
    {
      op.filter(img, dst);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
     */
    
    // Checks the destination image type
    dst = op.filter(img, null);
    harness.check(dst.getType(), op.createCompatibleDestImage(img, null).getType());
  }
}

