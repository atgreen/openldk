/* setPixel.java -- some checks for the setPixel() methods in the SampleModel
       class.
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

// Tags: JDK1.4

package gnu.testlet.java.awt.image.SampleModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.DataBuffer;
import java.awt.image.SampleModel;
import java.awt.image.SinglePixelPackedSampleModel;
import java.util.Arrays;

public class setPixel implements Testlet
{
  public void test(TestHarness harness)
  {
    testMethod1(harness);
    testMethod2(harness);
    testMethod3(harness);
  }
  
  public void testMethod1(TestHarness harness)
  {
    harness.checkPoint("(int, int, int[], DataBuffer)");
    SampleModel m = new SinglePixelPackedSampleModel(DataBuffer.TYPE_BYTE, 10, 
            20, new int[] { 224, 28, 3 });
    DataBuffer db = m.createDataBuffer();
    int[] pixel = new int[3];
    m.getPixel(1, 2, pixel, db);
    harness.check(Arrays.equals(pixel, new int[] {0, 0, 0}));
    m.setPixel(1, 2, new int[] {1, 2, 3}, db);
    m.getPixel(1, 2, pixel, db);
    harness.check(Arrays.equals(pixel, new int[] {1, 2, 3}));
    
    // try null array
    boolean pass = true;
    try
    {
      m.setPixel(1, 2, (int[]) null, db);
    }
    catch (NullPointerException e)
    {
        pass = true;
    }
    harness.check(pass);
    
    // try null data buffer
    pass = false;
    try
    {
      m.setPixel(1, 2, pixel, null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(int, int, float[], DataBuffer)");     
    SampleModel m = new SinglePixelPackedSampleModel(DataBuffer.TYPE_BYTE, 10, 
            20, new int[] { 224, 28, 3 });
    DataBuffer db = m.createDataBuffer();
    float[] pixel = new float[3];
    m.getPixel(1, 2, pixel, db);
    harness.check(Arrays.equals(pixel, new float[] {0, 0, 0}));
    m.setPixel(1, 2, new float[] {1, 2, 3}, db);
    m.getPixel(1, 2, pixel, db);
    harness.check(Arrays.equals(pixel, new float[] {1, 2, 3}));
    
    // try null array
    boolean pass = true;
    try
    {
      m.setPixel(1, 2, (float[]) null, db);
    }
    catch (NullPointerException e)
    {
        pass = true;
    }
    harness.check(pass);
    
    // try null data buffer
    pass = false;
    try
    {
      m.setPixel(1, 2, pixel, null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  public void testMethod3(TestHarness harness)
  {
    harness.checkPoint("(int, int, double[], DataBuffer)");     
    SampleModel m = new SinglePixelPackedSampleModel(DataBuffer.TYPE_BYTE, 10, 
            20, new int[] { 224, 28, 3 });
    DataBuffer db = m.createDataBuffer();
    double[] pixel = new double[3];
    m.getPixel(1, 2, pixel, db);
    harness.check(Arrays.equals(pixel, new double[] {0, 0, 0}));
    m.setPixel(1, 2, new double[] {1, 2, 3}, db);
    m.getPixel(1, 2, pixel, db);
    harness.check(Arrays.equals(pixel, new double[] {1, 2, 3}));
    
    // try null array
    boolean pass = true;
    try
    {
      m.setPixel(1, 2, (double[]) null, db);
    }
    catch (NullPointerException e)
    {
        pass = true;
    }
    harness.check(pass);
    
    // try null data buffer
    pass = false;
    try
    {
      m.setPixel(1, 2, pixel, null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
