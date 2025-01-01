/* getBounds2D.java -- some checks for the getBounds2D() method of the
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

/**
 * Checks the getBounds2D method in the
 * {@link BandCombineOp} class.
 */
public class getBounds2D implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("getBounds2D");
    
    //  This is a simple test; the BandCombineOp should not change the
    // dimensions of the raster
    
    float[][] matrix = new float[][] {{2, 7}};
    WritableRaster src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 5, 5, 1, new Point(0,0));
    BandCombineOp op = new BandCombineOp(matrix, null);
    harness.check(op.getBounds2D(src), src.getBounds());
    
    // This should throw an exception, as the bands in the source do not
    // match the rows in the matrix
    /*
     * The spec says it *MAY* throw an exception; the ref. impl does not do so...
    src = Raster.createBandedRaster(DataBuffer.TYPE_INT, 5, 5, 3, new Point(0,0));
    try
    {
      harness.check(op.getBounds2D(src), src.getBounds());
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    */
  }
}

