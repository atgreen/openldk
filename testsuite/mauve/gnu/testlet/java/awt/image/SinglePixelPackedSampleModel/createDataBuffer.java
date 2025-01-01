// Tags: JDK1.2

// Copyright (C) 2004 Sascha Brawer <brawer@dandelis.ch>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.awt.image.SinglePixelPackedSampleModel;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.DataBufferInt;
import java.awt.image.DataBufferUShort;
import java.awt.image.SinglePixelPackedSampleModel;


/**
 * Checks whether SinglePixelPackedSampleModel.createDataBuffer works.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class createDataBuffer
  implements Testlet
{
  public void test(TestHarness harness)
  {
    testInt(harness);
    testUShort(harness);
    testByte(harness);
  }


  private void testInt(TestHarness harness)
  {
    SinglePixelPackedSampleModel sm;
    DataBuffer dbuf;
    int[] bitMasks;

    harness.checkPoint("TYPE_INT");

    bitMasks = new int[] { 0xff00, 0xff };
    sm = new SinglePixelPackedSampleModel(DataBuffer.TYPE_INT,
                                          51, 83, bitMasks);
    dbuf = sm.createDataBuffer();

    // Check #1
    harness.check(dbuf instanceof DataBufferInt);

    // Check #2
    harness.check(dbuf.getDataType(), DataBuffer.TYPE_INT);

    // Check #3
    harness.check(dbuf.getNumBanks(), 1);

    // Check #4
    harness.check(dbuf.getOffset(), 0);

    // Check #5
    harness.check(dbuf.getSize(), /* width * height */ 51 * 83);

    // Check #6
    sm = new SinglePixelPackedSampleModel(DataBuffer.TYPE_INT,
                                          51, 83, /* stride */ 91,
                                          bitMasks);
    dbuf = sm.createDataBuffer();
    harness.check(dbuf.getSize(),
                  /* (stride * (height - 1)) + width */ 91 * 82 + 51);
  }


  private void testUShort(TestHarness harness)
  {
    SinglePixelPackedSampleModel sm;
    DataBuffer dbuf;
    int[] bitMasks;

    harness.checkPoint("TYPE_USHORT");

    bitMasks = new int[] { 0x0f00, 0xf000 };
    sm = new SinglePixelPackedSampleModel(DataBuffer.TYPE_USHORT,
                                          42, 10, bitMasks);
    dbuf = sm.createDataBuffer();

    // Check #1
    harness.check(dbuf instanceof DataBufferUShort);

    // Check #2
    harness.check(dbuf.getDataType(), DataBuffer.TYPE_USHORT);

    // Check #3
    harness.check(dbuf.getNumBanks(), 1);

    // Check #4
    harness.check(dbuf.getOffset(), 0);

    // Check #5
    harness.check(dbuf.getSize(), /* width * height */ 42 * 10);

    // Check #6
    sm = new SinglePixelPackedSampleModel(DataBuffer.TYPE_USHORT,
                                          42, 10, /* stride */ 31,
                                          bitMasks);
    dbuf = sm.createDataBuffer();
    harness.check(dbuf.getSize(),
                  /* (stride * (height - 1)) + width */ 31 * 9 + 42);
  }


  private void testByte(TestHarness harness)
  {
    SinglePixelPackedSampleModel sm;
    DataBuffer dbuf;
    int[] bitMasks;

    harness.checkPoint("TYPE_BYTE");

    bitMasks = new int[] { 0xf0, 0x08, 0x6, 0x1 };
    sm = new SinglePixelPackedSampleModel(DataBuffer.TYPE_BYTE,
                                          5, 3, bitMasks);
    dbuf = sm.createDataBuffer();

    // Check #1
    harness.check(dbuf instanceof DataBufferByte);

    // Check #2
    harness.check(dbuf.getDataType(), DataBuffer.TYPE_BYTE);

    // Check #3
    harness.check(dbuf.getNumBanks(), 1);

    // Check #4
    harness.check(dbuf.getOffset(), 0);

    // Check #5
    harness.check(dbuf.getSize(), /* width * height */ 5 * 3);

    // Check #6
    sm = new SinglePixelPackedSampleModel(DataBuffer.TYPE_USHORT,
                                          5, 3, /* stride */ 7,
                                          bitMasks);
    dbuf = sm.createDataBuffer();
    harness.check(dbuf.getSize(),
                  /* (stride * (height - 1)) + width */ 7 * 2 + 5);
  }
}
