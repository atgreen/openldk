/* getPixel.java -- some checks for the getPixel() methods in the SampleModel
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
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.SampleModel;
import java.awt.image.SinglePixelPackedSampleModel;
import java.util.Arrays;

public class getPixel implements Testlet
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
    
    // if the incoming array is null, a new one is created
    pixel = m.getPixel(1, 2, (int[]) null, db);
    harness.check(Arrays.equals(pixel, new int[] {1, 2, 3}));
    
    // try null data buffer
    boolean pass = false;
    try
    {
      m.getPixel(1, 2, pixel, null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try with a MultiPixelPackedSampleModel
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_BYTE, 10, 20, 2);
    db = m.createDataBuffer();
    db.setElem(0, 27);
    harness.check(m.getPixel(0, 0, (int[]) null, db)[0], 0);
    harness.check(m.getPixel(1, 0, (int[]) null, db)[0], 1);
    harness.check(m.getPixel(2, 0, (int[]) null, db)[0], 2);
    harness.check(m.getPixel(3, 0, (int[]) null, db)[0], 3);
    db.setElem(3, 27);
    harness.check(m.getPixel(0, 1, (int[]) null, db)[0], 0);
    harness.check(m.getPixel(1, 1, (int[]) null, db)[0], 1);
    harness.check(m.getPixel(2, 1, (int[]) null, db)[0], 2);
    harness.check(m.getPixel(3, 1, (int[]) null, db)[0], 3);
    db.setElem(6, 0x18);
    db.setElem(9, 0x30);
    db.setElem(12, 0x1C);
    harness.check(m.getPixel(1, 2, (int[]) null, db)[0], 1);
    harness.check(m.getPixel(2, 2, (int[]) null, db)[0], 2);
    harness.check(m.getPixel(1, 3, (int[]) null, db)[0], 3);
    harness.check(m.getPixel(2, 3, (int[]) null, db)[0], 0);
    harness.check(m.getPixel(1, 4, (int[]) null, db)[0], 1);
    harness.check(m.getPixel(2, 4, (int[]) null, db)[0], 3);
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
    m.setPixel(1, 2, new int[] {1, 2, 3}, db);
    m.getPixel(1, 2, pixel, db);
    harness.check(Arrays.equals(pixel, new float[] {1, 2, 3}));
    
    // if the incoming array is null, a new one is created
    pixel = m.getPixel(1, 2, (float[]) null, db);
    harness.check(Arrays.equals(pixel, new float[] {1, 2, 3}));
    
    // try null data buffer
    boolean pass = false;
    try
    {
      m.getPixel(1, 2, pixel, null);
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
    m.setPixel(1, 2, new int[] {1, 2, 3}, db);
    m.getPixel(1, 2, pixel, db);
    harness.check(Arrays.equals(pixel, new double[] {1, 2, 3}));
    
    // if the incoming array is null, a new one is created
    pixel = m.getPixel(1, 2, (double[]) null, db);
    harness.check(Arrays.equals(pixel, new double[] {1, 2, 3}));
    
    // try null data buffer
    boolean pass = false;
    try
    {
      m.getPixel(1, 2, pixel, null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
