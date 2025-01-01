// Tags: JDK1.2 

// Copyright (C) 2006 Francis Kung <fkung@redhat.com>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version. 

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.image.DirectColorModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Point;
import java.awt.image.BandedSampleModel;
import java.awt.image.ColorModel;
import java.awt.image.ComponentSampleModel;
import java.awt.image.DataBuffer;
import java.awt.image.DirectColorModel;
import java.awt.image.PixelInterleavedSampleModel;
import java.awt.image.Raster;
import java.awt.image.SampleModel;
import java.awt.image.SinglePixelPackedSampleModel;
import java.awt.image.WritableRaster;

/**
 * Some checks for the coerceData method in the {@link DirectColorModel} class.
 */
public class coerceData implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    harness.checkPoint("coerceData with BandedSampleModel");
    runTest(harness, new BandedSampleModel(DataBuffer.TYPE_INT,
                                           50, 50, 4));

    harness.checkPoint("coerceData with ComponentSampleModel");
    runTest(harness, new ComponentSampleModel(DataBuffer.TYPE_INT,
                                              50, 50, 4, 200,
                                              new int[]{0, 1, 2, 3}));

    // MultiPixelPackedSampleModel not allowed, since that sample model can
    // only represent one-banded images.  We should check to ensure failure
    // is consistent.
    
    harness.checkPoint("coerceData with PixelInterleavedSampleModel");
    runTest(harness, new PixelInterleavedSampleModel(DataBuffer.TYPE_INT,
                                                     50, 50, 4, 200,
                                                     new int[]{0, 1, 2, 3}));
    
    harness.checkPoint("coerceData with SinglePixelPackedSampleModel");
    runTest(harness, new SinglePixelPackedSampleModel(DataBuffer.TYPE_INT,
                                                      50, 50,
                                                      new int[]{0x00ff0000,
                                                                0x0000ff00,
                                                                0x000000ff,
                                                                0xff000000}));
  }
  
  private void runTest(TestHarness harness, SampleModel sample)
  {
    // Create and popular raster for testing
    WritableRaster rast = Raster.createWritableRaster(sample, new Point(0,0));
    
    for (int x = 0; x < 50; x++)
      for (int y = 0; y < 50; y++)
        for (int b = 0; b < 4; b++)
          if (b < 3)
            rast.setSample(x, y, b, (float)x+y+b);
          else
            rast.setSample(x, y, b, (float)((x+y)));

    DirectColorModel dcm = new DirectColorModel(32, 0x00ff0000, 0x0000ff00,
                                                0x000000ff, 0xff000000);
    
    harness.check(dcm.isAlphaPremultiplied(), false);
    
    // Check to ensure data is not changed needlessly
    ColorModel resultCM = dcm.coerceData(rast, false);
    harness.check(resultCM.getClass().equals(dcm.getClass()));
    harness.check(resultCM.isAlphaPremultiplied(), false);
    harness.check(dcm, resultCM);
    for (int x = 0; x < 50; x++)
      for (int y = 0; y < 50; y++)
        for (int b = 0; b < 4; b++)
          {
            if (b < 3)
              harness.check(rast.getSample(x, y, b), (x+y+b));
            else
              harness.check(rast.getSampleFloat(x, y, b), (x+y));
          }
    
    // Coerce data into a premultiplied state
    resultCM = dcm.coerceData(rast, true);
    
    // Ensure we're still using the same color model!
    harness.check(resultCM.getClass().equals(dcm.getClass()));
    harness.check(resultCM.isAlphaPremultiplied(), true);
    harness.check(! (dcm == resultCM));     // object is cloned...
    
    // Check values
    for (int x = 0; x < 50; x++)
      for (int y = 0; y < 50; y++)
        for (int b = 0; b < 4; b++)
          {
            if (b < 3)
              harness.check(rast.getSample(x, y, b),
                            Math.round((x+y+b)*((x+y)/(float)255)));
            else
              harness.check(rast.getSampleFloat(x, y, b), x+y);
          }

    // Make a copy of the raster, to compare against for the next test
    WritableRaster rast2 = Raster.createWritableRaster(sample, new Point(0,0));
    for (int x = 0; x < 50; x++)
      for (int y = 0; y < 50; y++)
        for (int b = 0; b < 4; b++)
          rast2.setSample(x, y, b,
                          rast.getSample(x, y, b));
    
    // And do the reverse.. un-multiply
    harness.check(resultCM.isAlphaPremultiplied(), true);
    ColorModel resultCM2 = resultCM.coerceData(rast, false);
    harness.check(resultCM2.getClass().equals(resultCM.getClass()));
    harness.check(resultCM2.isAlphaPremultiplied(), false);
    harness.check(! (resultCM == resultCM2));
    
    // Check values
    for (int x = 0; x < 50; x++)
      for (int y = 0; y < 50; y++)
        for (int b = 0; b < 4; b++)
          {
              if (b < 3) 
                {
                  // Do some trickery to work around differing floating-point
                  // precision (a division equalling 0.49999 will round up or
                  // down unpredictably)
                  float expected = rast2.getSample(x, y, b)/((x+y)/((float)255));
                  if (expected - (int)expected > 0.49
                      && expected - (int)expected < 0.51)
                    harness.check(rast.getSample(x, y, b) == (int)expected
                                  || rast.getSample(x, y, b) == (int)expected + 1);
                  else
                    harness.check(rast.getSample(x, y, b),Math.round(expected));
              }
              else
                harness.check(rast.getSampleFloat(x, y, b), (x+y));
          }
  }
}
