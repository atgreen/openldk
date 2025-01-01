/* getPixelBitStride.java -- some checks for the getPixelBitStride() method 
       in the MultiPixelPackedSampleModel class.
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

public class getPixelBitStride implements Testlet
{
  public void test(TestHarness harness)
  {
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
          DataBuffer.TYPE_INT, 10, 20, 8);
    harness.check(m.getPixelBitStride(), 8);
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 1);
    harness.check(m.getPixelBitStride(), 1);  
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 2);
    harness.check(m.getPixelBitStride(), 2);  
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 4);
    harness.check(m.getPixelBitStride(), 4);  
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 16);
    harness.check(m.getPixelBitStride(), 16);  
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 32);
    harness.check(m.getPixelBitStride(), 32);  
  }
}
