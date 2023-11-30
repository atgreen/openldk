// Tags: JDK1.2

// Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.image.SinglePixelPackedSampleModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.DataBufferInt;
import java.awt.image.DataBufferUShort;
import java.awt.image.SinglePixelPackedSampleModel;

/**
 * Some checks for the <code>setPixel()</code> method in the 
 * {@link SinglePixelPackedSampleModel} class.
 */
public class setPixel implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testByte(harness);
    testUShort(harness);
    testInt(harness);
  }

  private void testByte(TestHarness harness) 
  {
    harness.checkPoint("(int, int, Object, DataBuffer(Byte))");
    SinglePixelPackedSampleModel m1 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_BYTE, 2, 3, new int[] { 0xF0, 0x0F }
    );
    byte[] b = new byte[] { (byte) 0x11, (byte) 0x22, (byte) 0x33, (byte) 0x44, (byte) 0x55, (byte) 0x66 };
    DataBuffer db = new DataBufferByte(b, 6);  

    // set a value
    m1.setPixel(0, 0, new int[] { 0x0C, 0x07 }, db);
    m1.setPixel(1, 0, new int[] { 0x0B, 0x08 }, db);
    m1.setPixel(0, 1, new int[] { 0x0A, 0x09 }, db);
    m1.setPixel(1, 1, new int[] { 0x09, 0x0A }, db);
    m1.setPixel(0, 2, new int[] { 0x08, 0x0B }, db);
    m1.setPixel(1, 2, new int[] { 0x07, 0x0C }, db);
    harness.check(db.getElem(0), 0xC7);
    harness.check(db.getElem(1), 0xB8);
    harness.check(db.getElem(2), 0xA9);
    harness.check(db.getElem(3), 0x9A);
    harness.check(db.getElem(4), 0x8B);
    harness.check(db.getElem(5), 0x7C);
 
    // set a value with non-standard scanline stride 
    SinglePixelPackedSampleModel m2 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_BYTE, 2, 2, 3, new int[] { 0xF0, 0x0F }
    );
    m2.setPixel(0, 0, new int[] { 0x04, 0x01 }, db);
    m2.setPixel(1, 0, new int[] { 0x03, 0x02 }, db);
    m2.setPixel(0, 1, new int[] { 0x02, 0x03 }, db);
    m2.setPixel(1, 1, new int[] { 0x01, 0x04 }, db);
    harness.check(db.getElem(0), 0x41);
    harness.check(db.getElem(1), 0x32);
    harness.check(db.getElem(3), 0x23);
    harness.check(db.getElem(4), 0x14);
   
    // set a value with x < 0
    try
    {
      m1.setPixel(-1, 0, new int[] { 0x10, 0x01 }, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
 
    // set a value with y < 0
    try
    {
      m1.setPixel(0, -1, new int[] { 0x10, 0x01 }, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
   
    // set a value with transfer array too small
    try
    {
      m1.setPixel(0, 0, new int[] {}, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
   
    // set a value with null data buffer
    try
    {
      m1.setPixel(0, 0, new int[] { 0x10, 0x01 }, null);
      harness.check(false);  // should not get here
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }
   
  }

  private void testUShort(TestHarness harness) 
  {
    harness.checkPoint("(int, int, Object, DataBuffer(UShort))");
    SinglePixelPackedSampleModel m1 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_USHORT, 2, 3, new int[] { 0xFF00, 0x00FF }
    );
    short[] s = new short[] { (short) 0x1111, (short) 0x2222, (short) 0x3333, (short) 0x4444, (short) 0x5555, (short) 0x6666 };
    DataBuffer db = new DataBufferUShort(s, 6);  

    // set a value
    m1.setPixel(0, 0, new int[] { 0x00CC, 0x0077 }, db);
    m1.setPixel(1, 0, new int[] { 0x00BB, 0x0088 }, db);
    m1.setPixel(0, 1, new int[] { 0x00AA, 0x0099 }, db);
    m1.setPixel(1, 1, new int[] { 0x0099, 0x00AA }, db);
    m1.setPixel(0, 2, new int[] { 0x0088, 0x00BB }, db);
    m1.setPixel(1, 2, new int[] { 0x0077, 0x00CC }, db);
    harness.check(db.getElem(0), 0xCC77);
    harness.check(db.getElem(1), 0xBB88);
    harness.check(db.getElem(2), 0xAA99);
    harness.check(db.getElem(3), 0x99AA);
    harness.check(db.getElem(4), 0x88BB);
    harness.check(db.getElem(5), 0x77CC);
   
    // set a value with non-standard scanline stride 
    SinglePixelPackedSampleModel m2 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_USHORT, 2, 2, 3, new int[] { 0xFF00, 0x00FF }
    );
    m2.setPixel(0, 0, new int[] { 0x0044, 0x0011 }, db);
    m2.setPixel(1, 0, new int[] { 0x0033, 0x0022 }, db);
    m2.setPixel(0, 1, new int[] { 0x0022, 0x0033 }, db);
    m2.setPixel(1, 1, new int[] { 0x0011, 0x0044 }, db);
    harness.check(db.getElem(0), 0x4411);
    harness.check(db.getElem(1), 0x3322);
    harness.check(db.getElem(3), 0x2233);
    harness.check(db.getElem(4), 0x1144);
     
    // set a value with x < 0
    try
    {
      m1.setPixel(-1, 0, new int[] { 0x10, 0x01 }, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
   
    // set a value with y < 0
    try
    {
      m1.setPixel(0, -1, new int[] { 0x10, 0x01 }, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
     
    // set a value with transfer array too small
    try
    {
      m1.setPixel(0, 0, new int[] {}, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
     
    // set a value with null data buffer
    try
    {
      m1.setPixel(0, 0, new int[] { 0x10, 0x01 }, null);
      harness.check(false);  // should not get here
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }
  }

  private void testInt(TestHarness harness) 
  {
    harness.checkPoint("(int, int, Object, DataBuffer(Int))");
    SinglePixelPackedSampleModel m1 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_INT, 2, 3, new int[] { 0xFFFF0000, 0x0000FFFF }
    );
    int[] i = new int[] { 0x1111, 0x2222, 0x3333, 0x4444, 0x5555, 0x6666 };
    DataBuffer db = new DataBufferInt(i, 6);  

    // set a value
    m1.setPixel(0, 0, new int[] { 0x00CC, 0x0077 }, db);
    m1.setPixel(1, 0, new int[] { 0x00BB, 0x0088 }, db);
    m1.setPixel(0, 1, new int[] { 0x00AA, 0x0099 }, db);
    m1.setPixel(1, 1, new int[] { 0x0099, 0x00AA }, db);
    m1.setPixel(0, 2, new int[] { 0x0088, 0x00BB }, db);
    m1.setPixel(1, 2, new int[] { 0x0077, 0x00CC }, db);
    harness.check(db.getElem(0), 0x00CC0077);
    harness.check(db.getElem(1), 0x00BB0088);
    harness.check(db.getElem(2), 0x00AA0099);
    harness.check(db.getElem(3), 0x009900AA);
    harness.check(db.getElem(4), 0x008800BB);
    harness.check(db.getElem(5), 0x007700CC);
     
    // set a value with non-standard scanline stride 
    SinglePixelPackedSampleModel m2 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_INT, 2, 2, 3, new int[] { 0xFFFF0000, 0x0000FFFF }
    );
    m2.setPixel(0, 0, new int[] { 0x0044, 0x0011 }, db);
    m2.setPixel(1, 0, new int[] { 0x0033, 0x0022 }, db);
    m2.setPixel(0, 1, new int[] { 0x0022, 0x0033 }, db);
    m2.setPixel(1, 1, new int[] { 0x0011, 0x0044 }, db);
    harness.check(db.getElem(0), 0x00440011);
    harness.check(db.getElem(1), 0x00330022);
    harness.check(db.getElem(3), 0x00220033);
    harness.check(db.getElem(4), 0x00110044);
       
    // set a value with x < 0
    try
    {
      m1.setPixel(-1, 0, new int[] { 0x10, 0x01 }, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
     
    // set a value with y < 0
    try
    {
      m1.setPixel(0, -1, new int[] { 0x10, 0x01 }, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
       
    // set a value with transfer array too small
    try
    {
      m1.setPixel(0, 0, new int[] {}, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
       
    // set a value with null data buffer
    try
    {
      m1.setPixel(0, 0, new int[] { 0x10, 0x01 }, null);
      harness.check(false);  // should not get here
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }
  }
  
}

