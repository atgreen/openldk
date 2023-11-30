/* constructors.java -- some checks for the constructors in the 
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

import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.image.AffineTransformOp;
import java.awt.image.ImagingOpException;

/**
 * Some checks for the constructors in the {@link AffineTransformOp} class.
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
  }
  
  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("(xform, interpolationType)");

    // Simple test
    AffineTransform xform = new AffineTransform();
    AffineTransformOp op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    
    harness.check(op.getTransform(), xform);
    
    harness.check(op.getInterpolationType(), AffineTransformOp.TYPE_BICUBIC);
    harness.check(op.getRenderingHints(), new RenderingHints(RenderingHints.KEY_INTERPOLATION,
                                                             RenderingHints.VALUE_INTERPOLATION_BICUBIC));
    
    op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BILINEAR);
    harness.check(op.getTransform(), xform);
    harness.check(op.getInterpolationType(), AffineTransformOp.TYPE_BILINEAR);
    harness.check(op.getRenderingHints(), new RenderingHints(RenderingHints.KEY_INTERPOLATION,
                                                             RenderingHints.VALUE_INTERPOLATION_BILINEAR));

    op = new AffineTransformOp(xform, AffineTransformOp.TYPE_NEAREST_NEIGHBOR);
    harness.check(op.getTransform(), xform);
    harness.check(op.getInterpolationType(), AffineTransformOp.TYPE_NEAREST_NEIGHBOR);
    harness.check(op.getRenderingHints(), new RenderingHints(RenderingHints.KEY_INTERPOLATION,
                                                             RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR));

    
    // Try creating with invalid transofrm
    xform = new AffineTransform(0, 0, 0, 0, 0, 0);
    
    try
    {
      new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
      harness.check(false);
    }
    catch (ImagingOpException e)
    {
      harness.check(true);
    }
    
  }

  public void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(xform, hints)");

    // Simple test
    RenderingHints hints = new RenderingHints(RenderingHints.KEY_COLOR_RENDERING,
                                              RenderingHints.VALUE_COLOR_RENDER_QUALITY);
    AffineTransform xform = new AffineTransform();
    AffineTransformOp op = new AffineTransformOp(xform, hints);
    
    harness.check(op.getTransform(), xform);
    harness.check(op.getRenderingHints(), hints);
    
    // Try creating with invalid transofrm
    xform = new AffineTransform(0, 0, 0, 0, 0, 0);
    
    try
    {
      new AffineTransformOp(xform, hints);
      harness.check(false);
    }
    catch (ImagingOpException e)
    {
      harness.check(true);
    }
  }
}

