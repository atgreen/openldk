/* createCompatibleSampleModel.java -- some checks for the 
       createCompatibleSampleModel() method in the MultiPixelPackedSampleModel
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

package gnu.testlet.java.awt.image.MultiPixelPackedSampleModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.DataBuffer;
import java.awt.image.MultiPixelPackedSampleModel;

public class createCompatibleSampleModel implements Testlet
{
  public void test(TestHarness harness)
  {
    MultiPixelPackedSampleModel m1 = new MultiPixelPackedSampleModel(
        DataBuffer.TYPE_INT, 10, 20, 8);
    MultiPixelPackedSampleModel m2 
        = (MultiPixelPackedSampleModel) m1.createCompatibleSampleModel(5, 10);
    harness.check(m2.getDataType(), DataBuffer.TYPE_INT);          // check 1
    harness.check(m2.getWidth(), 5);                                // check 2
    harness.check(m2.getHeight(), 10);                              // check 3
    harness.check(m2.getNumBands(), 1);                             // check 4
    harness.check(m2.getScanlineStride(), 2);
    
    // if 'w' is <= 0 there should be an IllegalArgumentException
    try
    {
      /* SampleModel m3 = */ m1.createCompatibleSampleModel(0, 10);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
       
    // if 'h' is <= 0 there should be an IllegalArgumentException
    try
    {
      /* SampleModel m4 = */ m1.createCompatibleSampleModel(5, 0);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
  }
}
