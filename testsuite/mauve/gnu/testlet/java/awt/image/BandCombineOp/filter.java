/* filter.java -- some checks for the filter() method of the
              BandCombineOp class.
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

package gnu.testlet.java.awt.image.BandCombineOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Point;
import java.awt.image.BandCombineOp;
import java.awt.image.DataBuffer;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.util.Arrays;

/**
 * Checks the filter method in the {@link BandCombineOp} class.
 */
public class filter implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("filter");

    float[][] matrix = new float[][] {{2, 7},
                                      {5, 6}};
    
    WritableRaster src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 5, 5, 2, new Point(0,0));
    
    /* Setting up:
           [0]  [1]  [2]  [3]  [4]
      [0]   .    .    x    .    .
      [1]   .    .    .    .    .   
      [2]   .    .    .    .    . 
      [3]   .    .    .    .    .   
      [4]   .    .    .    .    x
      with two bands   
    */
    src.setSample(2, 1, 0, 150);
    src.setSample(4, 4, 0, 85);
    src.setSample(2, 1, 1, 25);
    src.setSample(4, 4, 1, 110);
    
    
    // Basic checks on output
    BandCombineOp op = new BandCombineOp(matrix, null);
    WritableRaster dst2 = op.createCompatibleDestRaster(src);
    WritableRaster dst = op.filter(src, dst2);
    harness.check(dst, dst2);
    harness.check(dst.getNumBands(), 2);
    
    // A null dst2 should also work
    dst2 = null;
    dst = op.filter(src, dst2);
    harness.check(dst instanceof WritableRaster);
    harness.check(dst2, null);
    
    // Check first band
    int[] pixels = dst.getSamples(0, 0, 5, 5, 0, (int[])null);

    int[] expected = new int[25];
    Arrays.fill(expected, 0);
    expected[7] = 475;
    expected[24] = 940;
    harness.check(Arrays.equals(expected, pixels));

    // Check second band
    pixels = dst.getSamples(0, 0, 5, 5, 1, pixels);
    expected[7] = 900;
    expected[24] = 1085;
    harness.check(Arrays.equals(expected, pixels));
    
    // Check the implied 1 at the end of the band samples, which happens if
    // there is one more column in the matrix than there are bands
    // And throw in a check with negative numbers too... why not =)
    matrix = new float[][] {{2, -7, 5},
                            {5, 6, -3}};
    op = new BandCombineOp(matrix, null);
    dst = op.filter(src, dst);
    
    // First band
    pixels = dst.getSamples(0, 0, 5, 5, 0, (int[])null);
    Arrays.fill(expected, 5);
    expected[7] = 130;
    expected[24] = -595;
    harness.check(Arrays.equals(expected, pixels));
    // Second band
    pixels = dst.getSamples(0, 0, 5, 5, 1, (int[])null);
    Arrays.fill(expected, -3);
    expected[7] = 897;
    expected[24] = 1082;
    harness.check(Arrays.equals(expected, pixels));
    
    // Check for overflow (this should fail silently and not throw an exception)
    matrix = new float[][] {{2000000000, 2000000000},
                            {2000000000, 2000000000}};
    try
    {
      op = new BandCombineOp(matrix, null);
      dst = op.filter(src, dst);
      harness.check(true);
    }
    catch (Exception e)
    {
      harness.check(false);
    }
    
    // Check for exceptions
    try
    {
      expected[25] = 100;
      harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
    
    // accessing invalid band number
    try
    {
      pixels = dst.getSamples(0, 0, 5, 5, 2, pixels);
      harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
    
    // dst has wrong number of bands
    dst = Raster.createBandedRaster(DataBuffer.TYPE_INT, 5, 5, 6, new Point(0,0)); 
    try
    {
      dst = op.filter(src, dst);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    // dst has wrong data type
    dst = Raster.createBandedRaster(DataBuffer.TYPE_BYTE, 5, 5, 1, new Point(0,0)); 
    try
    {
      dst = op.filter(src, dst);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
  }
}

