/* getTransferType.java -- some checks for the getTransferType() method in the
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

import java.awt.image.DataBuffer;
import java.awt.image.MultiPixelPackedSampleModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class getTransferType implements Testlet
{
  public void test(TestHarness harness)
  {
    testBYTE(harness);
    testUSHORT(harness);
    testINT(harness);
  }
  
  public void testBYTE(TestHarness harness)
  {
    harness.checkPoint("testBYTE()");
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
           DataBuffer.TYPE_BYTE, 10, 20, 1);
    harness.check(m.getTransferType(), DataBuffer.TYPE_BYTE);
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_BYTE, 10, 20, 2);
    harness.check(m.getTransferType(), DataBuffer.TYPE_BYTE);
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_BYTE, 10, 20, 4);
    harness.check(m.getTransferType(), DataBuffer.TYPE_BYTE);
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_BYTE, 10, 20, 8);
    harness.check(m.getTransferType(), DataBuffer.TYPE_BYTE);
  }

  public void testUSHORT(TestHarness harness)
  {
    harness.checkPoint("testUSHORT()");
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
           DataBuffer.TYPE_USHORT, 10, 20, 1);
    harness.check(m.getTransferType(), DataBuffer.TYPE_BYTE);
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_USHORT, 10, 20, 2);
    harness.check(m.getTransferType(), DataBuffer.TYPE_BYTE);
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_USHORT, 10, 20, 4);
    harness.check(m.getTransferType(), DataBuffer.TYPE_BYTE);
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_USHORT, 10, 20, 8);
    harness.check(m.getTransferType(), DataBuffer.TYPE_BYTE);
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_USHORT, 10, 20, 16);
    harness.check(m.getTransferType(), DataBuffer.TYPE_USHORT);
  }

  public void testINT(TestHarness harness)
  {
    harness.checkPoint("testINT()");
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
           DataBuffer.TYPE_INT, 10, 20, 1);
    harness.check(m.getTransferType(), DataBuffer.TYPE_BYTE);
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 2);
    harness.check(m.getTransferType(), DataBuffer.TYPE_BYTE);
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 4);
    harness.check(m.getTransferType(), DataBuffer.TYPE_BYTE);
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 8);
    harness.check(m.getTransferType(), DataBuffer.TYPE_BYTE);
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 16);
    harness.check(m.getTransferType(), DataBuffer.TYPE_USHORT);
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 32);
    harness.check(m.getTransferType(), DataBuffer.TYPE_INT);
  }

}
