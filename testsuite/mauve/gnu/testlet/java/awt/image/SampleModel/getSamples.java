/* getSamples.java -- some checks for the getSamples() methods in the 
       SampleModel class.
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

public class getSamples implements Testlet
{
  public void test(TestHarness harness)
  {
    testMethod1(harness);
    testMethod2(harness);
    testMethod3(harness);
  }
  
  public void testMethod1(TestHarness harness)
  {
    harness.checkPoint("(int, int, int, int, int, int[], DataBuffer)");
    SampleModel m = new SinglePixelPackedSampleModel(DataBuffer.TYPE_BYTE, 10, 
            20, new int[] { 224, 28, 3 });
    DataBuffer db = m.createDataBuffer();
    int[] samples = new int[6];
    m.getSamples(1, 2, 2, 3, 1, samples, db);
    harness.check(Arrays.equals(samples, new int[] {0, 0, 0, 0, 0, 0}));
    m.setPixels(1, 2, 2, 3, new int[] {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
            13, 14, 15, 16, 17, 18}, db);
    m.getSamples(1, 2, 2, 3, 0, samples, db);
    harness.check(Arrays.equals(samples, new int[] {1, 4, 7, 2, 5, 0}));
    m.getSamples(1, 2, 2, 3, 1, samples, db);
    harness.check(Arrays.equals(samples, new int[] {2, 5, 0, 3, 6, 1}));
    m.getSamples(1, 2, 2, 3, 2, samples, db);
    harness.check(Arrays.equals(samples, new int[] {3, 2, 1, 0, 3, 2}));
    
    // if the incoming array is null, a new one is created
    samples = m.getSamples(1, 2, 2, 3, 2, (int[]) null, db);
    harness.check(Arrays.equals(samples, new int[] {3, 2, 1, 0, 3, 2}));
    
    // try invalid band
    boolean pass = false;
    try
    {
      m.getSamples(1, 2, 2, 3, 3, samples, db);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try null data buffer
    pass = false;
    try
    {
      m.getSamples(1, 2, 2, 3, 0, samples, null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(int, int, int, int, int, float[], DataBuffer)");     
    SampleModel m = new SinglePixelPackedSampleModel(DataBuffer.TYPE_BYTE, 10, 
            20, new int[] { 224, 28, 3 });
    DataBuffer db = m.createDataBuffer();
    float[] samples = new float[6];
    m.getSamples(1, 2, 2, 3, 1, samples, db);
    harness.check(Arrays.equals(samples, new float[] {0, 0, 0, 0, 0, 0}));
    m.setPixels(1, 2, 2, 3, new int[] {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
            13, 14, 15, 16, 17, 18}, db);
    m.getSamples(1, 2, 2, 3, 0, samples, db);
    harness.check(Arrays.equals(samples, new float[] {1, 4, 7, 2, 5, 0}));
    m.getSamples(1, 2, 2, 3, 1, samples, db);
    harness.check(Arrays.equals(samples, new float[] {2, 5, 0, 3, 6, 1}));
    m.getSamples(1, 2, 2, 3, 2, samples, db);
    harness.check(Arrays.equals(samples, new float[] {3, 2, 1, 0, 3, 2}));
    
    // if the incoming array is null, a new one is created
    samples = m.getSamples(1, 2, 2, 3, 2, (float[]) null, db);
    harness.check(Arrays.equals(samples, new float[] {3, 2, 1, 0, 3, 2}));
    
    // try invalid band
    boolean pass = false;
    try
    {
      m.getSamples(1, 2, 2, 3, 3, samples, db);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try null data buffer
    pass = false;
    try
    {
      m.getSamples(1, 2, 2, 3, 0, samples, null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  public void testMethod3(TestHarness harness)
  {
    harness.checkPoint("(int, int, int, int, int, double[], DataBuffer)");     
    SampleModel m = new SinglePixelPackedSampleModel(DataBuffer.TYPE_BYTE, 10, 
            20, new int[] { 224, 28, 3 });
    DataBuffer db = m.createDataBuffer();
    double[] samples = new double[6];
    m.getSamples(1, 2, 2, 3, 1, samples, db);
    harness.check(Arrays.equals(samples, new double[] {0, 0, 0, 0, 0, 0}));
    m.setPixels(1, 2, 2, 3, new int[] {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
            13, 14, 15, 16, 17, 18}, db);
    m.getSamples(1, 2, 2, 3, 0, samples, db);
    harness.check(Arrays.equals(samples, new double[] {1, 4, 7, 2, 5, 0}));
    m.getSamples(1, 2, 2, 3, 1, samples, db);
    harness.check(Arrays.equals(samples, new double[] {2, 5, 0, 3, 6, 1}));
    m.getSamples(1, 2, 2, 3, 2, samples, db);
    harness.check(Arrays.equals(samples, new double[] {3, 2, 1, 0, 3, 2}));
    
    // if the incoming array is null, a new one is created
    samples = m.getSamples(1, 2, 2, 3, 2, (double[]) null, db);
    harness.check(Arrays.equals(samples, new double[] {3, 2, 1, 0, 3, 2}));
    
    // try invalid band
    boolean pass = false;
    try
    {
      m.getSamples(1, 2, 2, 3, 3, samples, db);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try null data buffer
    pass = false;
    try
    {
      m.getSamples(1, 2, 2, 3, 0, samples, null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
