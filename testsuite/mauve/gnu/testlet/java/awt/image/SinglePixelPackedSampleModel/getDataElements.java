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
 * Some checks for the <code>getDataElements()</code> method in the 
 * {@link SinglePixelPackedSampleModel} class.
 */
public class getDataElements implements Testlet 
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
      DataBuffer.TYPE_BYTE, 2, 2, new int[] { 224, 28, 3 }
    );
    byte[] b = new byte[] { (byte) 11, (byte) 22, (byte) 33, (byte) 44 };
    DataBuffer db = new DataBufferByte(b, 4);  
    
    byte[] de = (byte[]) m1.getDataElements(1, 1, null, db);
    harness.check(de.length, 1);                    // check 1
    harness.check(de[0], (byte) 44);                // check 2
    
    // check for coordinates out of bounds
    try                                             // check 3
    {
      m1.getDataElements(2, 2, null, db);
      harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }
    
    // check for return object of wrong dimension
    try                                             // check 4
    {
      m1.getDataElements(1, 1, new byte[0], db);
      harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }

    // check for return object of wrong type
    try                                            // check 5
    {
      m1.getDataElements(1, 1, new int[1], db);
      harness.check(false);
    }
    catch (ClassCastException e)
    {
      harness.check(true);
    }

    // check for null data buffer
    try                                            // check 6
    {
      m1.getDataElements(0, 0, null, null);
      harness.check(false);
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
      DataBuffer.TYPE_USHORT, 2, 2, new int[] { 224, 28, 3 }
    );
    short[] s = new short[] { (short) 11, (short) 22, (short) 33, (short) 44 };
    DataBuffer db = new DataBufferUShort(s, 4);  
    
    short[] de = (short[]) m1.getDataElements(1, 1, null, db);
    harness.check(de.length, 1);
    harness.check(de[0], (byte) 44);
    
    // check for coordinates out of bounds
    try
    {
      m1.getDataElements(2, 2, null, db);
      harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }

    // check for return object of wrong dimension
    try
    {
      m1.getDataElements(1, 1, new short[0], db);
      harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }

    // check for return object of wrong type
    try
    {
      m1.getDataElements(1, 1, new int[1], db);
      harness.check(false);
    }
    catch (ClassCastException e)
    {
      harness.check(true);
    }

    // check for null data buffer
    try
    {
      m1.getDataElements(0, 0, null, null);
      harness.check(false);
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
      DataBuffer.TYPE_INT, 2, 2, new int[] { 224, 28, 3 }
    );
    int[] i = new int[] { 11, 22, 33, 44 };
    DataBuffer db = new DataBufferInt(i, 4);  
      
    int[] de = (int[]) m1.getDataElements(1, 1, null, db);
    harness.check(de.length, 1);
    harness.check(de[0], 44);
      
    // check for coordinates out of bounds
    try
    {
      m1.getDataElements(2, 2, null, db);
      harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }

    // check for return object of wrong dimension
    try
    {
      m1.getDataElements(1, 1, new int[0], db);
      harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }

    // check for return object of wrong type
    try
    {
      m1.getDataElements(1, 1, new byte[1], db);
      harness.check(false);
    }
    catch (ClassCastException e)
    {
      harness.check(true);
    }

    // check for null data buffer
    try
    {
      m1.getDataElements(0, 0, null, null);
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }
  }
}

