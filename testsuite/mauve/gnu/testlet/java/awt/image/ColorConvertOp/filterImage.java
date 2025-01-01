/* filterImage.java -- some checks for the filter(Image) method of the
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
import java.awt.color.ICC_Profile;
import java.awt.geom.Line2D;
import java.awt.image.BufferedImage;
import java.awt.image.ColorConvertOp;

/**
 * Checks the filter(BufferedImage) method in the {@link ColorConvertOp} class.
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
    test1(harness);
    test2(harness);
    test3(harness);
    test4(harness);
  }
  
  private void test1(TestHarness harness)
  {
    harness.checkPoint("filter(BufferedImage) from ColorConvertOp(RenderingHints)");
    
    // Create an image to work on
    BufferedImage img = new BufferedImage(20, 20, BufferedImage.TYPE_USHORT_GRAY);
    Graphics2D g = (Graphics2D)img.getGraphics();
    g.draw(new Line2D.Double(0, 0, 20, 20));
    
    ColorConvertOp op = new ColorConvertOp(null);
    
    // Src and dst images can be the same (unlike some other Ops)
    try
    {
      op.filter(img, img);
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
      op.filter(img, dst);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    // Dst cannot be null for this ColorConvertOp
    try
    {
      op.filter(img, null);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    // Can I check that the actual filter operation happened properly?
  }
  
  private void test2(TestHarness harness)
  {
    harness.checkPoint("filter(BufferedImage) from ColorConvertOp(ColorSpace, RenderingHints)");
    
    // Create an image to work on
    BufferedImage img = new BufferedImage(20, 20, BufferedImage.TYPE_USHORT_GRAY);
    Graphics2D g = (Graphics2D)img.getGraphics();
    g.draw(new Line2D.Double(0, 0, 20, 20));
    
    ColorSpace cs = ColorSpace.getInstance(ColorSpace.CS_sRGB);
    ColorConvertOp op = new ColorConvertOp(cs, null);
    
    // Check null destination
    try
    {
      BufferedImage dst = op.filter(img, null);
      harness.check(dst.getColorModel().getColorSpace().getType(), ColorSpace.TYPE_RGB);
      // Any other checks to run?  Show the data was filtered properly?
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    // Check non-null destination
    try
    {
      BufferedImage dst = new BufferedImage(20, 20, BufferedImage.TYPE_INT_RGB);
      dst = op.filter(img, dst);
      harness.check(dst.getColorModel().getColorSpace().getType(), ColorSpace.TYPE_RGB);
      // Any other checks to run?
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    // Can we introduce an alpha?
    try
    {
      BufferedImage dst = new BufferedImage(20, 20, BufferedImage.TYPE_INT_ARGB);
      dst = op.filter(img, dst);
      harness.check(dst.getColorModel().getColorSpace().getType(), ColorSpace.TYPE_RGB);
      // Any other checks to run?
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    // Different destination type: this should end up as GRAY, via RGB
    // (but how can I test the intermediate step?)
    try
    {
      BufferedImage dst = new BufferedImage(20, 20, BufferedImage.TYPE_BYTE_GRAY);
      dst = op.filter(img, dst);
      harness.check(dst.getColorModel().getColorSpace().getType(), ColorSpace.TYPE_GRAY);
      // Any other checks to run?
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
  }
  
  private void test3(TestHarness harness)
  {
    harness.checkPoint("filter(BufferedImage) from ColorConvertOp(ColorSpace, ColorSpace, RenderingHints)");

    // Create an image to work on
    BufferedImage img = new BufferedImage(20, 20, BufferedImage.TYPE_USHORT_GRAY);
    Graphics2D g = (Graphics2D)img.getGraphics();
    g.draw(new Line2D.Double(0, 0, 20, 20));
    
    ColorSpace cs1 = ColorSpace.getInstance(ColorSpace.CS_CIEXYZ);
    //ColorSpace cs2 = ColorSpace.getInstance(ColorSpace.CS_PYCC);
       // PYCC is not implemented
    ColorSpace cs2 = ColorSpace.getInstance(ColorSpace.CS_sRGB);
    ColorConvertOp op = new ColorConvertOp(cs1, cs2, null);

    // Simpler tests (ie, src != dest) are skipped, assume they work here if
    // they worked earlier
    
    try
    {
      BufferedImage dst = op.filter(img, null);
      harness.check(dst.getColorModel().getColorSpace().getType(), ColorSpace.TYPE_RGB);
      // PYCC would have been ColorSpace.TYPE_3CLR
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
   
    try
    {
      BufferedImage dst = new BufferedImage(20, 20, BufferedImage.TYPE_INT_ARGB_PRE);
      op.filter(img, dst);
      harness.check(dst.getColorModel().getColorSpace().getType(), ColorSpace.TYPE_RGB);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    // Can I check that the actual filter operation happened properly?
  }
  
  private void test4(TestHarness harness)
  {
    harness.checkPoint("filter(BufferedImage) from ColorConvertOp(ICC_Profile[], RenderingHints)");
    // Create an image to work on
    BufferedImage img = new BufferedImage(20, 20, BufferedImage.TYPE_USHORT_GRAY);
    Graphics2D g = (Graphics2D)img.getGraphics();
    g.draw(new Line2D.Double(0, 0, 20, 20));
    
    ICC_Profile[] profile = new ICC_Profile[] {ICC_Profile.getInstance(ColorSpace.CS_LINEAR_RGB),
                                               ICC_Profile.getInstance(ColorSpace.CS_CIEXYZ),
                                               ICC_Profile.getInstance(ColorSpace.CS_sRGB)};
    ColorConvertOp op = new ColorConvertOp(profile, null);
    
    try
    {
      BufferedImage dst = op.filter(img, null);
      harness.check(dst.getColorModel().getColorSpace().getType(), ColorSpace.TYPE_RGB);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    try
    {
      BufferedImage dst = new BufferedImage(20, 20, BufferedImage.TYPE_BYTE_GRAY);
      dst = op.filter(img, dst);
      harness.check(dst.getColorModel().getColorSpace().getType(), ColorSpace.TYPE_GRAY);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    // Can I check that the actual filter operation happened properly?
  }
}

