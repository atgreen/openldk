/* constructors.java -- some checks for the constructors in the 
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

// Tags: JDK1.2

package gnu.testlet.java.awt.image.RescaleOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.RescaleOp;
import java.util.Arrays;

/**
 * Some checks for the constructors in the {@link RescaleOp} class.
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
    harness.checkPoint("(float[], float[], RenderingHints)");

    // Simple test
    float[] scale = new float[] {0.6f};
    float[] offset = new float[] {1.1f};
    RescaleOp op = new RescaleOp(scale, offset, null);
    
    harness.check(Arrays.equals(op.getScaleFactors(null), scale));
    harness.check(Arrays.equals(op.getOffsets(null), offset));
    
    harness.check(op.getRenderingHints(), null);

    scale = new float[] {0.6f, 1.2f, 5.6f, 2.2f};
    offset = new float[] {1.1f, 3f, 2.7f, 8.0f};
    op = new RescaleOp(scale, offset, null);
    
    harness.check(Arrays.equals(op.getScaleFactors(null), scale));
    harness.check(Arrays.equals(op.getOffsets(null), offset));
    harness.check(op.getRenderingHints(), null);

    // Null arguments
    try
    {
      op = new RescaleOp(null, offset, null);
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }
    
    try
    {
      op = new RescaleOp(scale, null, null);
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }
    
    try
    {
      op = new RescaleOp(null, null, null);
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }
    
    // Empty arrays
    try
    {
      op = new RescaleOp(new float[]{}, new float[]{}, null);
      harness.check(true);
    }
    catch (NullPointerException e)
    {
      harness.check(false);
    }
    
    // Mis-matched array lengths are allowed for now
    scale = new float[] {1f, 2f, 3f, 4f, 5f, 6f};
    offset = new float[] {1f, 2f, 3f};
    try
    {
      op = new RescaleOp(scale, offset, null);
      harness.check(true);
    }
    catch (IllegalArgumentException ex)
    {
      harness.check(false);
    }
    
    // Negative values
    scale = new float[] {1f, -2f};
    offset = new float[] {2f, -1f,};
    try
    {
      op = new RescaleOp(scale, offset, null);
      harness.check(true);
    }
    catch (IllegalArgumentException ex)
    {
      harness.check(false);
    }
  }

  public void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(float, float, RenderingHints)");

    // Simple test
    RescaleOp op = new RescaleOp(2f, 6.5f, null);
    
    harness.check(Arrays.equals(op.getScaleFactors(null), new float[]{2f}));
    harness.check(Arrays.equals(op.getOffsets(null), new float[]{6.5f}));
    harness.check(op.getRenderingHints(), null);
    
    // Negative values
    try
    {
      op = new RescaleOp(-5f, -2f, null);
      harness.check(true);
    }
    catch (IllegalArgumentException ex)
    {
      harness.check(false);
    }
  }
}

