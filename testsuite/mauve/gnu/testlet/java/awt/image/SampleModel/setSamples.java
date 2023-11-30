/* setSamples.java -- some checks for the setSamples() methods in the 
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

public class setSamples implements Testlet
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
    m.setSamples(1, 2, 2, 3, 0, new int[] {1, 2, 3, 4 ,5 ,6}, db);
    m.getSamples(1, 2, 2, 3, 0, samples, db);
    harness.check(Arrays.equals(samples, new int[] {1, 2, 3, 4, 5, 6}));
    m.setSamples(1, 2, 2, 3, 1, new int[] {7, 8, 9, 10, 11, 12}, db);
    m.getSamples(1, 2, 2, 3, 1, samples, db);
    harness.check(Arrays.equals(samples, new int[] {7, 0, 1, 2, 3, 4}));
    m.setSamples(1, 2, 2, 3, 2, new int[] {13, 14, 15, 16, 17, 18}, db);
    m.getSamples(1, 2, 2, 3, 2, samples, db);
    harness.check(Arrays.equals(samples, new int[] {1, 2, 3, 0, 1, 2}));
    
    // try invalid band
    boolean pass = false;
    try
    {
      m.setSamples(1, 2, 2, 3, 3, samples, db);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try null sample data
    pass = false;
    try
    {
      m.setSamples(1, 2, 2, 3, 0, (int[]) null, db);
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
    m.setSamples(1, 2, 2, 3, 0, new float[] {1, 2, 3, 4 ,5 ,6}, db);
    m.getSamples(1, 2, 2, 3, 0, samples, db);
    harness.check(Arrays.equals(samples, new float[] {1, 2, 3, 4, 5, 6}));
    m.setSamples(1, 2, 2, 3, 1, new float[] {7, 8, 9, 10, 11, 12}, db);
    m.getSamples(1, 2, 2, 3, 1, samples, db);
    harness.check(Arrays.equals(samples, new float[] {7, 0, 1, 2, 3, 4}));
    m.setSamples(1, 2, 2, 3, 2, new float[] {13, 14, 15, 16, 17, 18}, db);
    m.getSamples(1, 2, 2, 3, 2, samples, db);
    harness.check(Arrays.equals(samples, new float[] {1, 2, 3, 0, 1, 2}));
    
    // try invalid band
    boolean pass = false;
    try
    {
      m.setSamples(1, 2, 2, 3, 3, samples, db);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try null sample data
    pass = false;
    try
    {
      m.setSamples(1, 2, 2, 3, 0, (float[]) null, db);
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
    m.setSamples(1, 2, 2, 3, 0, new double[] {1, 2, 3, 4 ,5 ,6}, db);
    m.getSamples(1, 2, 2, 3, 0, samples, db);
    harness.check(Arrays.equals(samples, new double[] {1, 2, 3, 4, 5, 6}));
    m.setSamples(1, 2, 2, 3, 1, new double[] {7, 8, 9, 10, 11, 12}, db);
    m.getSamples(1, 2, 2, 3, 1, samples, db);
    harness.check(Arrays.equals(samples, new double[] {7, 0, 1, 2, 3, 4}));
    m.setSamples(1, 2, 2, 3, 2, new double[] {13, 14, 15, 16, 17, 18}, db);
    m.getSamples(1, 2, 2, 3, 2, samples, db);
    harness.check(Arrays.equals(samples, new double[] {1, 2, 3, 0, 1, 2}));
    
    // try invalid band
    boolean pass = false;
    try
    {
      m.setSamples(1, 2, 2, 3, 3, samples, db);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try null sample data
    pass = false;
    try
    {
      m.setSamples(1, 2, 2, 3, 0, (double[]) null, db);
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
      m.getSamples(1, 2, 2, 3, 0, samples, null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
