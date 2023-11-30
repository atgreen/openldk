/* createDataBuffer.java -- some checks for the createDataBuffer() method in 
       the MultiPixelPackedSampleModel class.
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

public class createDataBuffer implements Testlet
{
  public void test(TestHarness harness)
  {
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
        DataBuffer.TYPE_INT, 10, 20, 8);
    DataBuffer db = m.createDataBuffer();
    harness.check(db.getDataType(), DataBuffer.TYPE_INT);
    harness.check(db.getNumBanks(), 1);
    harness.check(db.getSize(), 60);

    MultiPixelPackedSampleModel m2 = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_INT, 10, 20, 8, 4, 16);
    DataBuffer db2 = m2.createDataBuffer();
    harness.check(db2.getDataType(), DataBuffer.TYPE_INT);
    harness.check(db2.getNumBanks(), 1);
    harness.check(db2.getSize(), 81);

    MultiPixelPackedSampleModel m3 = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_BYTE, 10, 20, 8);
    DataBuffer db3 = m3.createDataBuffer();
    harness.check(db3.getDataType(), DataBuffer.TYPE_BYTE);
    harness.check(db3.getNumBanks(), 1);
    harness.check(db3.getSize(), 200);

    MultiPixelPackedSampleModel m4 = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_BYTE, 10, 20, 8, 11, 16);
    DataBuffer db4 = m4.createDataBuffer();
    harness.check(db4.getDataType(), DataBuffer.TYPE_BYTE);
    harness.check(db4.getNumBanks(), 1);
    harness.check(db4.getSize(), 222);
  
    MultiPixelPackedSampleModel m5 = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_USHORT, 10, 20, 8);
    DataBuffer db5 = m5.createDataBuffer();
    harness.check(db5.getDataType(), DataBuffer.TYPE_USHORT);
    harness.check(db5.getNumBanks(), 1);
    harness.check(db5.getSize(), 100);

    MultiPixelPackedSampleModel m6 = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_USHORT, 10, 20, 8, 6, 16);
    DataBuffer db6 = m6.createDataBuffer();
    harness.check(db6.getDataType(), DataBuffer.TYPE_USHORT);
    harness.check(db6.getNumBanks(), 1);
    harness.check(db6.getSize(), 121);
  }
}
