/* getSample.java -- some checks for the getSample() method in the SampleModel
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

public class getSample implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("(int, int, int, DataBuffer)");
    SampleModel m = new SinglePixelPackedSampleModel(DataBuffer.TYPE_BYTE, 10, 
            20, new int[] { 224, 28, 3 });
    DataBuffer db = m.createDataBuffer();
    harness.check(m.getSample(1, 2, 0, db), 0);
    harness.check(m.getSample(1, 2, 1, db), 0);
    harness.check(m.getSample(1, 2, 2, db), 0);
    m.setPixel(1, 2, new int[] {1, 2, 3}, db);
    harness.check(m.getSample(1, 2, 0, db), 1);
    harness.check(m.getSample(1, 2, 1, db), 2);
    harness.check(m.getSample(1, 2, 2, db), 3);
    
    // try band out of range
    boolean pass = false;
    try
    {
      m.getSample(1, 2, -1, db);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(true);
    
    pass = false;
    try
    {
      m.getSample(1, 2, 3, db);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(true);
    
    // try null data buffer
    pass = false;
    try
    {
      m.getSample(1, 2, 0, null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
  
}
