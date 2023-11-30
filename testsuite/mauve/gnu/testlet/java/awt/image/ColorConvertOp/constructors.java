/* constructors.java -- some checks for the constructors in the 
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

import java.awt.RenderingHints;
import java.awt.color.ColorSpace;
import java.awt.color.ICC_Profile;
import java.awt.image.ColorConvertOp;
import java.util.Arrays;

/**
 * Some checks for the constructors in the {@link ColorConvertOp} class.
 */
public class constructors implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)      
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
  }
  
  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("(ColorSpace, ColorSpace, RenderingHints)");

    // Simple test
    ColorSpace srcCs = ColorSpace.getInstance(ColorSpace.CS_sRGB);
    ColorSpace dstCs = ColorSpace.getInstance(ColorSpace.CS_GRAY);
    RenderingHints hints = new RenderingHints(RenderingHints.KEY_DITHERING,
                                              RenderingHints.VALUE_DITHER_ENABLE);
    hints.put(RenderingHints.KEY_COLOR_RENDERING,
              RenderingHints.VALUE_COLOR_RENDER_QUALITY);
    ColorConvertOp op = new ColorConvertOp(srcCs, dstCs, hints);
    
    harness.check(op.getICC_Profiles(), null);
    harness.check(op.getRenderingHints(), hints);

    
    // Null arguments
    op = new ColorConvertOp(srcCs, dstCs, null);
    
    harness.check(op.getICC_Profiles(), null);
    harness.check(op.getRenderingHints(), null);

    try
    {
      op = new ColorConvertOp(null, dstCs, null);
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }
    
    try
    {
      op = new ColorConvertOp(srcCs, null, null);
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }
    
    try
    {
      op = new ColorConvertOp(null, null, null);
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }
    
  }

  public void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(ColorSpace, RenderingHints)");

    // Simple test
    ColorSpace cs = ColorSpace.getInstance(ColorSpace.CS_sRGB);
    RenderingHints hints = new RenderingHints(RenderingHints.KEY_DITHERING,
                                              RenderingHints.VALUE_DITHER_ENABLE);
    hints.put(RenderingHints.KEY_COLOR_RENDERING,
              RenderingHints.VALUE_COLOR_RENDER_QUALITY);
    ColorConvertOp op = new ColorConvertOp(cs, hints);
    
    harness.check(op.getICC_Profiles(), null);
    harness.check(op.getRenderingHints(), hints);
    
    // Null arguments
    op = new ColorConvertOp(cs, null);
    
    harness.check(op.getICC_Profiles(), null);
    harness.check(op.getRenderingHints(), null);

    try
    {
      op = new ColorConvertOp((ColorSpace)null, (RenderingHints)null);
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }
    
  }

  public void testConstructor3(TestHarness harness)
  {
    harness.checkPoint("(ICC_Profile[], RenderingHints)");

    // Simple test
    ICC_Profile[] profile = new ICC_Profile[] {ICC_Profile.getInstance(ColorSpace.CS_LINEAR_RGB),
                                               ICC_Profile.getInstance(ColorSpace.CS_CIEXYZ),
                                               ICC_Profile.getInstance(ColorSpace.CS_sRGB),
                                               ICC_Profile.getInstance(ColorSpace.CS_PYCC)};
    
    RenderingHints hints = new RenderingHints(RenderingHints.KEY_DITHERING,
                                              RenderingHints.VALUE_DITHER_ENABLE);
    hints.put(RenderingHints.KEY_COLOR_RENDERING,
              RenderingHints.VALUE_COLOR_RENDER_QUALITY);
    ColorConvertOp op = new ColorConvertOp(profile, hints);
    
    // Work around lack of ICC_Profile.equals()
    harness.check(Arrays.equals(op.getICC_Profiles(), profile));
    harness.check(op.getRenderingHints(), hints);

    // Empty or too few profiles are not caught until filter(), so they are
    // allowed here
    try
    {
      op = new ColorConvertOp(new ICC_Profile[0], null);
      harness.check(Arrays.equals(op.getICC_Profiles(), new ICC_Profile[0]));
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    try
    {
      profile = new ICC_Profile[]{ICC_Profile.getInstance(ColorSpace.CS_LINEAR_RGB)};
      op = new ColorConvertOp(profile, null);
      harness.check(Arrays.equals(op.getICC_Profiles(), profile));
    }
    catch (IllegalArgumentException e)
    {
      harness.check(false);
    }
    
    // Null arguments
    op = new ColorConvertOp(profile, null);
    
    // Work around lack of ICC_Profile.equals()
    harness.check(Arrays.equals(op.getICC_Profiles(), profile));
    harness.check(op.getRenderingHints(), null);

    try
    {
      op = new ColorConvertOp((ICC_Profile[])null, null);
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }
    
  }
  
  public void testConstructor4(TestHarness harness)
  {
    harness.checkPoint("(RenderingHints)");

    // Simple test
    RenderingHints hints = new RenderingHints(RenderingHints.KEY_DITHERING,
                                              RenderingHints.VALUE_DITHER_ENABLE);
    hints.put(RenderingHints.KEY_COLOR_RENDERING,
              RenderingHints.VALUE_COLOR_RENDER_QUALITY);
    ColorConvertOp op = new ColorConvertOp(hints);
    
    harness.check(op.getICC_Profiles(), null);
    harness.check(op.getRenderingHints(), hints);
    
    // Null arguments
    op = new ColorConvertOp(null);
    
    harness.check(op.getICC_Profiles(), null);
    harness.check(op.getRenderingHints(), null);
  }
}

