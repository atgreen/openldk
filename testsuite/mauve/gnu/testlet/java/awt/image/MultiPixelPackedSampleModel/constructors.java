/* constructors.java -- some checks for the constructors in the 
       MultiPixelPackedSampleModel class.
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

package gnu.testlet.java.awt.image.MultiPixelPackedSampleModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.DataBuffer;
import java.awt.image.MultiPixelPackedSampleModel;

public class constructors implements Testlet
{
  public void test(TestHarness harness)
  {
    testConstructor1(harness);
    testConstructor2(harness);
  }
  
  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("(int, int, int, int)");
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_INT, 10, 20, 8);
    harness.check(m.getDataType(), DataBuffer.TYPE_INT);
    harness.check(m.getWidth(), 10);
    harness.check(m.getHeight(), 20);
    harness.check(m.getSampleSize(0), 8);
    harness.check(m.getNumBands(), 1);
    harness.check(m.getNumDataElements(), 1);
    harness.check(m.getScanlineStride(), 3);
    harness.check(m.getDataBitOffset(), 0);
    
    // unsupported type should throw IllegalArgumentException
    boolean pass = false;
    try
    {
      m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_DOUBLE, 10, 20, 8);  
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  public void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(int, int, int, int, int, int)");
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
        DataBuffer.TYPE_INT, 10, 20, 8, 5, 1);
    harness.check(m.getDataType(), DataBuffer.TYPE_INT);
    harness.check(m.getWidth(), 10);
    harness.check(m.getHeight(), 20);
    harness.check(m.getSampleSize(0), 8);
    harness.check(m.getNumBands(), 1);
    harness.check(m.getNumDataElements(), 1);
    harness.check(m.getScanlineStride(), 5);
    harness.check(m.getDataBitOffset(), 1);
  }
}
