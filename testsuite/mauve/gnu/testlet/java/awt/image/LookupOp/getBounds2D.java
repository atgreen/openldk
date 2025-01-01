/* getBounds2D.java -- some checks for the getBounds2D() methods in the
       LookupOp class.
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

package gnu.testlet.java.awt.image.LookupOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.ByteLookupTable;
import java.awt.image.DataBuffer;
import java.awt.image.LookupOp;
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.Raster;

public class getBounds2D implements Testlet
{
  public void test(TestHarness harness)
  {
    testMethod1(harness);
    testMethod2(harness);
  }
  
  public void testMethod1(TestHarness harness)
  {
    harness.checkPoint("(BufferedImage)");
    BufferedImage image = new BufferedImage(10, 20, 
            BufferedImage.TYPE_INT_ARGB);
    byte[] bytes = new byte[] {(byte) 0xAA, (byte) 0xBB};
    ByteLookupTable t = new ByteLookupTable(0, bytes);
    LookupOp op = new LookupOp(t, null);
    Rectangle2D bounds = op.getBounds2D(image);
    harness.check(bounds.getWidth(), 10);
    harness.check(bounds.getHeight(), 20);
    
    // try null argument
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
  
  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(Raster)");    
    byte[] bytes = new byte[] {(byte) 0xAA, (byte) 0xBB};
    ByteLookupTable t = new ByteLookupTable(0, bytes);
    LookupOp op = new LookupOp(t, null);
    Raster raster = Raster.createWritableRaster(
            new MultiPixelPackedSampleModel(DataBuffer.TYPE_BYTE, 10, 20, 8), 
            null);
    Rectangle2D bounds = op.getBounds2D(raster);
    harness.check(bounds.getWidth(), 10);
    harness.check(bounds.getHeight(), 20);
    
    // try null argument
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
}
