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
 * Some checks for the <code>setDataElements()</code> method in the 
 * {@link SinglePixelPackedSampleModel} class.
 */
public class setDataElements implements Testlet 
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
    m1.setDataElements(0, 0, new byte[] { (byte) 0x77 }, db);
    m1.setDataElements(1, 0, new byte[] { (byte) 0x88 }, db);
    m1.setDataElements(0, 1, new byte[] { (byte) 0x99 }, db);
    m1.setDataElements(1, 1, new byte[] { (byte) 0xAA }, db);
    m1.setDataElements(0, 2, new byte[] { (byte) 0xBB }, db);
    m1.setDataElements(1, 2, new byte[] { (byte) 0xCC }, db);
    harness.check(db.getElem(0), 0x77);
    harness.check(db.getElem(1), 0x88);
    harness.check(db.getElem(2), 0x99);
    harness.check(db.getElem(3), 0xAA);
    harness.check(db.getElem(4), 0xBB);
    harness.check(db.getElem(5), 0xCC);
    
    // set a value with non-standard scanline stride 
    SinglePixelPackedSampleModel m2 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_BYTE, 2, 2, 3, new int[] { 0xF0, 0x0F }
    );
    m2.setDataElements(0, 0, new byte[] { (byte) 0x11 }, db);
    m2.setDataElements(1, 0, new byte[] { (byte) 0x22 }, db);
    m2.setDataElements(0, 1, new byte[] { (byte) 0x33 }, db);
    m2.setDataElements(1, 1, new byte[] { (byte) 0x44 }, db);
    harness.check(db.getElem(0), 0x11);
    harness.check(db.getElem(1), 0x22);
    harness.check(db.getElem(3), 0x33);
    harness.check(db.getElem(4), 0x44);
      
    // set a value with x < 0
    try
    {
      m1.setDataElements(-1, 0, new byte[] { (byte) 0x99}, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
    
    // set a value with y < 0
    try
    {
      m1.setDataElements(0, -1, new byte[] { (byte) 0x99}, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
      
    // set a value with wrong transfer type
    try
    {
      m1.setDataElements(0, 0, new int[] { (int) 0x99}, db);
      harness.check(false);  // should not get here
    }
    catch (ClassCastException e)
    {
      harness.check(true);
    }
      
    // set a value with transfer array too small
    try
    {
      m1.setDataElements(0, 0, new byte[] {}, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
      
    // set a value with null data buffer
    try
    {
      m1.setDataElements(0, 0, new byte[] { (byte) 0x99 }, null);
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
    m1.setDataElements(0, 0, new short[] { (short) 0x7777 }, db);
    m1.setDataElements(1, 0, new short[] { (short) 0x8888 }, db);
    m1.setDataElements(0, 1, new short[] { (short) 0x9999 }, db);
    m1.setDataElements(1, 1, new short[] { (short) 0xAAAA }, db);
    m1.setDataElements(0, 2, new short[] { (short) 0xBBBB }, db);
    m1.setDataElements(1, 2, new short[] { (short) 0xCCCC }, db);
    harness.check(db.getElem(0), 0x7777);
    harness.check(db.getElem(1), 0x8888);
    harness.check(db.getElem(2), 0x9999);
    harness.check(db.getElem(3), 0xAAAA);
    harness.check(db.getElem(4), 0xBBBB);
    harness.check(db.getElem(5), 0xCCCC);
      
    // set a value with non-standard scanline stride 
    SinglePixelPackedSampleModel m2 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_USHORT, 2, 2, 3, new int[] { 0xFF00, 0x00FF }
    );
    m2.setDataElements(0, 0, new short[] { (short) 0x1111 }, db);
    m2.setDataElements(1, 0, new short[] { (short) 0x2222 }, db);
    m2.setDataElements(0, 1, new short[] { (short) 0x3333 }, db);
    m2.setDataElements(1, 1, new short[] { (short) 0x4444 }, db);
    harness.check(db.getElem(0), 0x1111);
    harness.check(db.getElem(1), 0x2222);
    harness.check(db.getElem(3), 0x3333);
    harness.check(db.getElem(4), 0x4444);
        
    // set a value with x < 0
    try
    {
      m1.setDataElements(-1, 0, new short[] { (short) 0x9999 }, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
      
    // set a value with y < 0
    try
    {
      m1.setDataElements(0, -1, new short[] { (short) 0x9999 }, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
        
    // set a value with wrong transfer type
    try
    {
      m1.setDataElements(0, 0, new int[] { (int) 0x9999 }, db);
      harness.check(false);  // should not get here
    }
    catch (ClassCastException e)
    {
      harness.check(true);
    }
        
    // set a value with transfer array too small
    try
    {
      m1.setDataElements(0, 0, new short[] {}, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
        
    // set a value with null data buffer
    try
    {
      m1.setDataElements(0, 0, new short[] { (short) 0x9999 }, null);
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
      DataBuffer.TYPE_INT, 2, 3, new int[] { 0xFFFF00, 0x00FFFF }
    );
    int[] i = new int[] { (int) 0x11111111, (int) 0x22222222, (int) 0x33333333, (int) 0x44444444, (int) 0x55555555, (int) 0x66666666 };
    DataBuffer db = new DataBufferInt(i, 6);  

    // set a value
    m1.setDataElements(0, 0, new int[] { (int) 0x77777777 }, db);
    m1.setDataElements(1, 0, new int[] { (int) 0x88888888 }, db);
    m1.setDataElements(0, 1, new int[] { (int) 0x99999999 }, db);
    m1.setDataElements(1, 1, new int[] { (int) 0xAAAAAAAA }, db);
    m1.setDataElements(0, 2, new int[] { (int) 0xBBBBBBBB }, db);
    m1.setDataElements(1, 2, new int[] { (int) 0xCCCCCCCC }, db);
    harness.check(db.getElem(0), 0x77777777);
    harness.check(db.getElem(1), 0x88888888);
    harness.check(db.getElem(2), 0x99999999);
    harness.check(db.getElem(3), 0xAAAAAAAA);
    harness.check(db.getElem(4), 0xBBBBBBBB);
    harness.check(db.getElem(5), 0xCCCCCCCC);
        
    // set a value with non-standard scanline stride 
    SinglePixelPackedSampleModel m2 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_INT, 2, 2, 3, new int[] { 0xFFFF00, 0x00FFFF }
    );
    m2.setDataElements(0, 0, new int[] { (int) 0x11111111 }, db);
    m2.setDataElements(1, 0, new int[] { (int) 0x22222222 }, db);
    m2.setDataElements(0, 1, new int[] { (int) 0x33333333 }, db);
    m2.setDataElements(1, 1, new int[] { (int) 0x44444444 }, db);
    harness.check(db.getElem(0), 0x11111111);
    harness.check(db.getElem(1), 0x22222222);
    harness.check(db.getElem(3), 0x33333333);
    harness.check(db.getElem(4), 0x44444444);
          
    // set a value with x < 0
    try
    {
      m1.setDataElements(-1, 0, new int[] { (int) 0x9999 }, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
        
    // set a value with y < 0
    try
    {
      m1.setDataElements(0, -1, new int[] { (int) 0x9999 }, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
          
    // set a value with wrong transfer type
    try
    {
      m1.setDataElements(0, 0, new short[] { (short) 0x9999 }, db);
      harness.check(false);  // should not get here
    }
    catch (ClassCastException e)
    {
      harness.check(true);
    }
          
    // set a value with transfer array too small
    try
    {
      m1.setDataElements(0, 0, new int[] {}, db);
      harness.check(false);  // should not get here
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
          
    // set a value with null data buffer
    try
    {
      m1.setDataElements(0, 0, new int[] { (int) 0x9999 }, null);
      harness.check(false);  // should not get here
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }
  }
}

