/* setDataElements.java -- some checks for the setDataElements() method in the
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

public class setDataElements implements Testlet
{
  public void test(TestHarness harness)
  {
    testTYPE_BYTE(harness);
    testTYPE_USHORT(harness);
    testBadTransferArray(harness);
    testNullTransferArray(harness);
  }
  
  public void testTYPE_BYTE(TestHarness harness)
  {
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_BYTE, 10, 20, 8);
    DataBuffer db = m.createDataBuffer();
    m.setDataElements(2, 1, new byte[] {(byte) 0xFA}, db);
    harness.check(db.getElem(12), 0xFA);
  }

  public void testTYPE_USHORT(TestHarness harness)
  {
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_USHORT, 10, 20, 8);
    DataBuffer db = m.createDataBuffer();
    m.setDataElements(0, 0, new byte[] {(byte) 0x12}, db);
    harness.check(db.getElem(0), 0x1200);
    m.setDataElements(1, 0, new byte[] {(byte) 0x34}, db);
    harness.check(db.getElem(0), 0x1234);
    m.setDataElements(2, 1, new byte[] {(byte) 0xAB}, db);
    harness.check(db.getElem(6), 0xAB00);
    m.setDataElements(3, 1, new byte[] {(byte) 0xCD}, db);
    harness.check(db.getElem(6), 0xABCD);
  }
  
  /**
   * Tests for the required ClassCastException if the incoming Object is not
   * an array of the TransferType.
   * 
   * @param harness  the test harness.
   */
  public void testBadTransferArray(TestHarness harness)
  {
    harness.checkPoint("testBadTransferArray()");
    
    // here a byte[] is expected...
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
              DataBuffer.TYPE_BYTE, 10, 20, 8);
    DataBuffer db = m.createDataBuffer();
    
    boolean pass = false;
    try
    {
      m.setDataElements(2, 1, new int[] {0xFA}, db);
    }
    catch (ClassCastException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // here a byte[] is expected...
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_USHORT, 10, 20, 8);
    db = m.createDataBuffer();
    
    pass = false;
    try
    {
      m.setDataElements(2, 1, new short[] {0xFA}, db);
    }
    catch (ClassCastException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // here a short[] is expected...
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_USHORT, 10, 20, 16);
    db = m.createDataBuffer();
    
    pass = false;
    try
    {
      m.setDataElements(2, 1, new byte[] {(byte) 0xFA}, db);
    }
    catch (ClassCastException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // here a short[] is expected...
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 16);
    db = m.createDataBuffer();
    
    pass = false;
    try
    {
      m.setDataElements(2, 1, new byte[] {(byte) 0xFA}, db);
    }
    catch (ClassCastException e)
    {
      pass = true;
    }
    harness.check(pass);
  
  }

  /**
   * Tests for the required NullPointerException if the incoming Object is null.
   * 
   * @param harness  the test harness.
   */
  public void testNullTransferArray(TestHarness harness)
  {
    harness.checkPoint("testNullTransferArray()");
    
    // here a byte[] is expected...
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
              DataBuffer.TYPE_BYTE, 10, 20, 8);
    DataBuffer db = m.createDataBuffer();
    
    boolean pass = false;
    try
    {
      m.setDataElements(2, 1, null, db);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // here a byte[] is expected...
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_USHORT, 10, 20, 8);
    db = m.createDataBuffer();
    
    pass = false;
    try
    {
      m.setDataElements(2, 1, null, db);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // here a short[] is expected...
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_USHORT, 10, 20, 16);
    db = m.createDataBuffer();
    
    pass = false;
    try
    {
      m.setDataElements(2, 1, null, db);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // here a short[] is expected...
    m = new MultiPixelPackedSampleModel(DataBuffer.TYPE_INT, 10, 20, 16);
    db = m.createDataBuffer();
    
    pass = false;
    try
    {
      m.setDataElements(2, 1, null, db);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  
  }


}
