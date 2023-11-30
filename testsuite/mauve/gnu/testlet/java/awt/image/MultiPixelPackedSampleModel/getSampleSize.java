/* getSampleSize.java -- some checks for the getSampleSize() method in the
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

public class getSampleSize implements Testlet
{
  public void test(TestHarness harness)
  {
    testMethod1(harness);
    testMethod2(harness);
  }
  
  public void testMethod1(TestHarness harness)
  {
    harness.checkPoint("()");
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
          DataBuffer.TYPE_INT, 10, 20, 1);
    harness.check(m.getSampleSize()[0], 1);
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 2);
    harness.check(m.getSampleSize()[0], 2);  
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 4);
    harness.check(m.getSampleSize()[0], 4);  
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 8);
    harness.check(m.getSampleSize()[0], 8);  
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 16);
    harness.check(m.getSampleSize()[0], 16);  
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 32);
    harness.check(m.getSampleSize()[0], 32);
    
    // check that the returned array can't be used to modify the model's 
    // internal state
    int[] sizes = m.getSampleSize();
    harness.check(sizes[0], 32);
    sizes[0] = 99;
    int[] sizes2 = m.getSampleSize();
    harness.check(sizes != sizes2);
    harness.check(sizes2[0], 32);
    
  }

  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(int)");
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
          DataBuffer.TYPE_INT, 10, 20, 1);
    harness.check(m.getSampleSize(0), 1);
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 2);
    harness.check(m.getSampleSize(0), 2);  
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 4);
    harness.check(m.getSampleSize(0), 4);  
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 8);
    harness.check(m.getSampleSize(0), 8);  
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 16);
    harness.check(m.getSampleSize(0), 16);  
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 32);
    harness.check(m.getSampleSize(0), 32);
    
    // try invalid band - the band is ignored
    harness.check(m.getSampleSize(-1), 32);
    harness.check(m.getSampleSize(1), 32);
  }
}
