/* filterImage.java -- some checks for the filter(Image) method of the
              ConvolveOp class.
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

import java.awt.Graphics2D;
import java.awt.geom.Line2D;
import java.awt.image.BufferedImage;
import java.awt.image.ConvolveOp;
import java.awt.image.Kernel;
import java.awt.image.WritableRaster;

/**
 * Checks the filter(BufferedImage) method in the {@link ConvolveOp} class.
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
    testFilter(harness);
    testPremultiplied(harness);
  }
  
  private void testDefaults(TestHarness harness)
  {
    harness.checkPoint("testDefaults");
    
    // Create an image to work on
    BufferedImage img = new BufferedImage(20, 20, BufferedImage.TYPE_USHORT_GRAY);
    Graphics2D g = (Graphics2D)img.getGraphics();
    g.draw(new Line2D.Double(0, 0, 20, 20));
    
    Kernel k1 = new Kernel(3, 3, new float[] {0.1f,0.2f,0.3f,
                                              0.4f,0.5f,0.6f,
                                              0.7f,0.8f,0.9f});
    ConvolveOp op = new ConvolveOp(k1, ConvolveOp.EDGE_ZERO_FILL, null);
    
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
      //op.filter(img, dst);
      harness.check(true);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
  }
  
  private void testFilter(TestHarness harness)
  {
    harness.checkPoint("testFilter");
    
    // Create an image to work on
    BufferedImage img = new BufferedImage(5, 5, BufferedImage.TYPE_INT_ARGB);
    WritableRaster r = img.getRaster();
    for (int i = 0; i < r.getHeight(); i++)
      for (int j = 0; j < r.getWidth(); j++)
        {
          r.setSample(j, i, 0, i * 5 + j * 8);
          r.setSample(j, i, 1, (r.getHeight() - i) * 5 + (r.getWidth() - j) * 8);
          r.setSample(j, i, 2, 150);
          if (i > (r.getHeight() / 2) && j > (r.getWidth() / 2))
            r.setSample(j, i, 3, 200);
          else
            r.setSample(j, i, 3, 0);
        }
    
    Kernel k1 = new Kernel(3, 3, new float[] {0.2f,0.3f,0.5f,
                                              0.4f,0.6f,0.7f,
                                              0.6f,0.9f,0.8f});
    ConvolveOp op = new ConvolveOp(k1, ConvolveOp.EDGE_ZERO_FILL, null);
    
    BufferedImage dst = op.filter(img, null);
    WritableRaster dest = dst.getRaster();
    
    harness.check(img.isAlphaPremultiplied(), false);
    harness.check(dst.isAlphaPremultiplied(), false);
    
    harness.check(dest.getSample(0, 0, 0), 0);
    harness.check(dest.getSample(1, 0, 0), 0);
    harness.check(dest.getSample(2, 0, 0), 0);
    harness.check(dest.getSample(3, 0, 0), 0);
    harness.check(dest.getSample(4, 0, 0), 0);
    harness.check(dest.getSample(0, 1, 0), 0);
    harness.check(dest.getSample(1, 1, 0), 52);
    harness.check(dest.getSample(2, 1, 0), 92);
    harness.check(dest.getSample(3, 1, 0), 132);
    harness.check(dest.getSample(4, 1, 0), 0);
    harness.check(dest.getSample(0, 2, 0), 0);
    harness.check(dest.getSample(1, 2, 0), 77);
    harness.check(dest.getSample(2, 2, 0), 117);
    harness.check(dest.getSample(3, 2, 0), 157);
    harness.check(dest.getSample(4, 2, 0), 0);
    harness.check(dest.getSample(0, 3, 0), 0);
    harness.check(dest.getSample(1, 3, 0), 102);
    harness.check(dest.getSample(2, 3, 0), 142);
    harness.check(dest.getSample(3, 3, 0), 182);
    harness.check(dest.getSample(4, 3, 0), 0);
    harness.check(dest.getSample(0, 4, 0), 0);
    harness.check(dest.getSample(1, 4, 0), 0);
    harness.check(dest.getSample(2, 4, 0), 0);
    harness.check(dest.getSample(3, 4, 0), 0);
    harness.check(dest.getSample(4, 4, 0), 0);
    
    harness.check(dest.getSample(0, 0, 1), 0);
    harness.check(dest.getSample(1, 0, 1), 0);
    harness.check(dest.getSample(2, 0, 1), 0);
    harness.check(dest.getSample(3, 0, 1), 0);
    harness.check(dest.getSample(4, 0, 1), 0);
    harness.check(dest.getSample(0, 1, 1), 0);
    harness.check(dest.getSample(1, 1, 1), 255);
    harness.check(dest.getSample(2, 1, 1), 232);
    harness.check(dest.getSample(3, 1, 1), 192);
    harness.check(dest.getSample(4, 1, 1), 0);
    harness.check(dest.getSample(0, 2, 1), 0);
    harness.check(dest.getSample(1, 2, 1), 247);
    harness.check(dest.getSample(2, 2, 1), 207);
    harness.check(dest.getSample(3, 2, 1), 167);
    harness.check(dest.getSample(4, 2, 1), 0);
    harness.check(dest.getSample(0, 3, 1), 0);
    harness.check(dest.getSample(1, 3, 1), 222);
    harness.check(dest.getSample(2, 3, 1), 182);
    harness.check(dest.getSample(3, 3, 1), 142);
    harness.check(dest.getSample(4, 3, 1), 0);
    harness.check(dest.getSample(0, 4, 1), 0);
    harness.check(dest.getSample(1, 4, 1), 0);
    harness.check(dest.getSample(2, 4, 1), 0);
    harness.check(dest.getSample(3, 4, 1), 0);
    harness.check(dest.getSample(4, 4, 1), 0);
    
    harness.check(dest.getSample(0, 0, 2), 0);
    harness.check(dest.getSample(1, 0, 2), 0);
    harness.check(dest.getSample(2, 0, 2), 0);
    harness.check(dest.getSample(3, 0, 2), 0);
    harness.check(dest.getSample(4, 0, 2), 0);
    harness.check(dest.getSample(0, 1, 2), 0);
    harness.check(dest.getSample(1, 1, 2), 255);
    harness.check(dest.getSample(2, 1, 2), 255);
    harness.check(dest.getSample(3, 1, 2), 255);
    harness.check(dest.getSample(4, 1, 2), 0);
    harness.check(dest.getSample(0, 2, 2), 0);
    harness.check(dest.getSample(1, 2, 2), 255);
    harness.check(dest.getSample(2, 2, 2), 255);
    harness.check(dest.getSample(3, 2, 2), 255);
    harness.check(dest.getSample(4, 2, 2), 0);
    harness.check(dest.getSample(0, 3, 2), 0);
    harness.check(dest.getSample(1, 3, 2), 255);
    harness.check(dest.getSample(2, 3, 2), 255);
    harness.check(dest.getSample(3, 3, 2), 255);
    harness.check(dest.getSample(4, 3, 2), 0);
    harness.check(dest.getSample(0, 4, 2), 0);
    harness.check(dest.getSample(1, 4, 2), 0);
    harness.check(dest.getSample(2, 4, 2), 0);
    harness.check(dest.getSample(3, 4, 2), 0);
    harness.check(dest.getSample(4, 4, 2), 0);
    
    harness.check(dest.getSample(0, 0, 3), 0);
    harness.check(dest.getSample(1, 0, 3), 0);
    harness.check(dest.getSample(2, 0, 3), 0);
    harness.check(dest.getSample(3, 0, 3), 0);
    harness.check(dest.getSample(4, 0, 3), 0);
    harness.check(dest.getSample(0, 1, 3), 0);
    harness.check(dest.getSample(1, 1, 3), 0);
    harness.check(dest.getSample(2, 1, 3), 0);
    harness.check(dest.getSample(3, 1, 3), 0);
    harness.check(dest.getSample(4, 1, 3), 0);
    harness.check(dest.getSample(0, 2, 3), 0);
    harness.check(dest.getSample(1, 2, 3), 0);
    harness.check(dest.getSample(2, 2, 3) - 39 >= 0      // allow rounding error
                  && dest.getSample(2, 2, 3) - 39 <= 1);
    harness.check(dest.getSample(3, 2, 3), 100);
    harness.check(dest.getSample(4, 2, 3), 0);
    harness.check(dest.getSample(0, 3, 3), 0);
    harness.check(dest.getSample(1, 3, 3), 0);
    harness.check(dest.getSample(2, 3, 3) - 119 >= 0     // allow rounding error
                  && dest.getSample(2, 3, 3) - 119 <= 1);
    harness.check(dest.getSample(3, 3, 3), 255);
    harness.check(dest.getSample(4, 3, 3), 0);
    harness.check(dest.getSample(0, 4, 3), 0);
    harness.check(dest.getSample(1, 4, 3), 0);
    harness.check(dest.getSample(2, 4, 3), 0);
    harness.check(dest.getSample(3, 4, 3), 0);
    harness.check(dest.getSample(4, 4, 3), 0);
  }
  
  // Test the pre-multiplied alpha stuff
  private void testPremultiplied(TestHarness harness)
  {
    harness.checkPoint("testPremultiplied");
    
    // Create an image to work on
    BufferedImage img = new BufferedImage(5, 5, BufferedImage.TYPE_INT_ARGB_PRE);
    WritableRaster r = img.getRaster();
    for (int i = 0; i < r.getHeight(); i++)
      for (int j = 0; j < r.getWidth(); j++)
        {
          r.setSample(j, i, 0, i * 5 + j * 8);
          r.setSample(j, i, 1, (r.getHeight() - i) * 5 + (r.getWidth() - j) * 8);
          r.setSample(j, i, 2, 150);
          if (i > (r.getHeight() / 2) && j > (r.getWidth() / 2))
            r.setSample(j, i, 3, 200);
          else
            r.setSample(j, i, 3, 0);
        }

    Kernel k1 = new Kernel(3, 3, new float[] {0.2f,0.3f,0.5f,
                                              0.4f,0.6f,0.7f,
                                              0.6f,0.9f,0.8f});
    ConvolveOp op = new ConvolveOp(k1, ConvolveOp.EDGE_ZERO_FILL, null);
    
    BufferedImage dst = new BufferedImage(5, 5, BufferedImage.TYPE_INT_ARGB);
    
    dst = op.filter(img, dst);
    WritableRaster dest = dst.getRaster();
    
    harness.check(img.isAlphaPremultiplied(), true);
    harness.check(dst.isAlphaPremultiplied(), false);

    // The following test values are COMPLETELY IDENTICAL to those in
    // testFilter, even though this does a premultiplied conversion and
    // testFilter does not.  It seems that the reference implementation
    // IGNORES the pre-multiplied status of images, contrary to the spec.
    
    harness.check(dest.getSample(0, 0, 0), 0);
    harness.check(dest.getSample(1, 0, 0), 0);
    harness.check(dest.getSample(2, 0, 0), 0);
    harness.check(dest.getSample(3, 0, 0), 0);
    harness.check(dest.getSample(4, 0, 0), 0);
    harness.check(dest.getSample(0, 1, 0), 0);
    harness.check(dest.getSample(1, 1, 0), 52);
    harness.check(dest.getSample(2, 1, 0), 92);
    harness.check(dest.getSample(3, 1, 0), 132);
    harness.check(dest.getSample(4, 1, 0), 0);
    harness.check(dest.getSample(0, 2, 0), 0);
    harness.check(dest.getSample(1, 2, 0), 77);
    harness.check(dest.getSample(2, 2, 0), 117);
    harness.check(dest.getSample(3, 2, 0), 157);
    harness.check(dest.getSample(4, 2, 0), 0);
    harness.check(dest.getSample(0, 3, 0), 0);
    harness.check(dest.getSample(1, 3, 0), 102);
    harness.check(dest.getSample(2, 3, 0), 142);
    harness.check(dest.getSample(3, 3, 0), 182);
    harness.check(dest.getSample(4, 3, 0), 0);
    harness.check(dest.getSample(0, 4, 0), 0);
    harness.check(dest.getSample(1, 4, 0), 0);
    harness.check(dest.getSample(2, 4, 0), 0);
    harness.check(dest.getSample(3, 4, 0), 0);
    harness.check(dest.getSample(4, 4, 0), 0);
    
    harness.check(dest.getSample(0, 0, 1), 0);
    harness.check(dest.getSample(1, 0, 1), 0);
    harness.check(dest.getSample(2, 0, 1), 0);
    harness.check(dest.getSample(3, 0, 1), 0);
    harness.check(dest.getSample(4, 0, 1), 0);
    harness.check(dest.getSample(0, 1, 1), 0);
    harness.check(dest.getSample(1, 1, 1), 255);
    harness.check(dest.getSample(2, 1, 1), 232);
    harness.check(dest.getSample(3, 1, 1), 192);
    harness.check(dest.getSample(4, 1, 1), 0);
    harness.check(dest.getSample(0, 2, 1), 0);
    harness.check(dest.getSample(1, 2, 1), 247);
    harness.check(dest.getSample(2, 2, 1), 207);
    harness.check(dest.getSample(3, 2, 1), 167);
    harness.check(dest.getSample(4, 2, 1), 0);
    harness.check(dest.getSample(0, 3, 1), 0);
    harness.check(dest.getSample(1, 3, 1), 222);
    harness.check(dest.getSample(2, 3, 1), 182);
    harness.check(dest.getSample(3, 3, 1), 142);
    harness.check(dest.getSample(4, 3, 1), 0);
    harness.check(dest.getSample(0, 4, 1), 0);
    harness.check(dest.getSample(1, 4, 1), 0);
    harness.check(dest.getSample(2, 4, 1), 0);
    harness.check(dest.getSample(3, 4, 1), 0);
    harness.check(dest.getSample(4, 4, 1), 0);
    
    harness.check(dest.getSample(0, 0, 2), 0);
    harness.check(dest.getSample(1, 0, 2), 0);
    harness.check(dest.getSample(2, 0, 2), 0);
    harness.check(dest.getSample(3, 0, 2), 0);
    harness.check(dest.getSample(4, 0, 2), 0);
    harness.check(dest.getSample(0, 1, 2), 0);
    harness.check(dest.getSample(1, 1, 2), 255);
    harness.check(dest.getSample(2, 1, 2), 255);
    harness.check(dest.getSample(3, 1, 2), 255);
    harness.check(dest.getSample(4, 1, 2), 0);
    harness.check(dest.getSample(0, 2, 2), 0);
    harness.check(dest.getSample(1, 2, 2), 255);
    harness.check(dest.getSample(2, 2, 2), 255);
    harness.check(dest.getSample(3, 2, 2), 255);
    harness.check(dest.getSample(4, 2, 2), 0);
    harness.check(dest.getSample(0, 3, 2), 0);
    harness.check(dest.getSample(1, 3, 2), 255);
    harness.check(dest.getSample(2, 3, 2), 255);
    harness.check(dest.getSample(3, 3, 2), 255);
    harness.check(dest.getSample(4, 3, 2), 0);
    harness.check(dest.getSample(0, 4, 2), 0);
    harness.check(dest.getSample(1, 4, 2), 0);
    harness.check(dest.getSample(2, 4, 2), 0);
    harness.check(dest.getSample(3, 4, 2), 0);
    harness.check(dest.getSample(4, 4, 2), 0);
    
    harness.check(dest.getSample(0, 0, 3), 0);
    harness.check(dest.getSample(1, 0, 3), 0);
    harness.check(dest.getSample(2, 0, 3), 0);
    harness.check(dest.getSample(3, 0, 3), 0);
    harness.check(dest.getSample(4, 0, 3), 0);
    harness.check(dest.getSample(0, 1, 3), 0);
    harness.check(dest.getSample(1, 1, 3), 0);
    harness.check(dest.getSample(2, 1, 3), 0);
    harness.check(dest.getSample(3, 1, 3), 0);
    harness.check(dest.getSample(4, 1, 3), 0);
    harness.check(dest.getSample(0, 2, 3), 0);
    harness.check(dest.getSample(1, 2, 3), 0);
    harness.check(dest.getSample(2, 2, 3) - 39 >= 0      // allow rounding error
                  && dest.getSample(2, 2, 3) - 39 <= 1);
    harness.check(dest.getSample(3, 2, 3), 100);
    harness.check(dest.getSample(4, 2, 3), 0);
    harness.check(dest.getSample(0, 3, 3), 0);
    harness.check(dest.getSample(1, 3, 3), 0);
    harness.check(dest.getSample(2, 3, 3) - 119 >= 0     // allow rounding error
                  && dest.getSample(2, 3, 3) - 119 <= 1);
    harness.check(dest.getSample(3, 3, 3), 255);
    harness.check(dest.getSample(4, 3, 3), 0);
    harness.check(dest.getSample(0, 4, 3), 0);
    harness.check(dest.getSample(1, 4, 3), 0);
    harness.check(dest.getSample(2, 4, 3), 0);
    harness.check(dest.getSample(3, 4, 3), 0);
    harness.check(dest.getSample(4, 4, 3), 0);
  }
}

