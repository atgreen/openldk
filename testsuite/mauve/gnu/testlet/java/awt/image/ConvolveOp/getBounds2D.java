/* getBounds2D.java -- some checks for the getBounds2D() methods in the
       ConvolveOp class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
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

// Tags: JDk1.4

package gnu.testlet.java.awt.image.ConvolveOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Rectangle;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.ConvolveOp;
import java.awt.image.DataBuffer;
import java.awt.image.Kernel;
import java.awt.image.Raster;

public class getBounds2D implements Testlet
{
  public void test(TestHarness harness)
  {
    testMethod1(harness);
  }
  
  public void testMethod1(TestHarness harness)
  {
    harness.checkPoint("(Raster)");
    Kernel k1 = new Kernel(3, 3, new float[] {1f, 2f, 3f, 4f, 5f, 6f, 7f, 8f, 
            9f});
    ConvolveOp op = new ConvolveOp(k1, ConvolveOp.EDGE_NO_OP, null);

    Raster r = Raster.createBandedRaster(DataBuffer.TYPE_BYTE, 4, 30, 3, null);
    Rectangle2D bounds = op.getBounds2D(r);
    harness.check(bounds, new Rectangle(0, 0, 4, 30));
      
    // try null raster
    boolean pass = false;
    try
    {
      op.getBounds2D((Raster) null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(BufferedImage)");
    Kernel k1 = new Kernel(3, 3, new float[] {1f, 2f, 3f, 4f, 5f, 6f, 7f, 8f, 
            9f});
    ConvolveOp op = new ConvolveOp(k1, ConvolveOp.EDGE_NO_OP, null);

    BufferedImage image = new BufferedImage(5, 10, BufferedImage.TYPE_BYTE_GRAY);
    Rectangle2D bounds = op.getBounds2D(image);
    harness.check(bounds, new Rectangle(0, 0, 5, 10));
      
    // try null raster
    boolean pass = false;
    try
    {
      op.getBounds2D((BufferedImage) null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

}
