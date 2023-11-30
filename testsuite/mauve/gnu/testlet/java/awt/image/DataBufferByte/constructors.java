//Tags: JDK1.2

//Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.image.DataBufferByte;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;

/**
 * Some tests for the constructors in the {@link DataBufferByte} class.
 */
public class constructors implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
    testConstructor5(harness);
    testConstructor6(harness);
  }

  private void testConstructor1(TestHarness harness) 
  {
    harness.checkPoint("DataBufferByte(int)");
    DataBufferByte b1 = new DataBufferByte(1);
    harness.check(b1.getDataType() == DataBuffer.TYPE_BYTE);
    harness.check(b1.getSize() == 1);
    harness.check(b1.getNumBanks() == 1);
    harness.check(b1.getOffset() == 0);
  
    DataBufferByte b2 = new DataBufferByte(0);
    harness.check(b2.getSize() == 0);
    harness.check(b2.getNumBanks() == 1);
    harness.check(b2.getOffset() == 0);
  
    boolean pass = false;
    try 
    {
      DataBufferByte b3 = new DataBufferByte(-1);
    }
    catch (NegativeArraySizeException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

  private void testConstructor2(TestHarness harness)   
  {
    harness.checkPoint("DataBufferByte(byte[][], int)");
    byte[][] source = new byte[][] {{1, 2}};
    DataBufferByte b = new DataBufferByte(source, 1);
    harness.check(b.getSize() == 1);
    harness.check(b.getNumBanks() == 1);
    harness.check(b.getOffset() == 0);
  
    // does a change to the source array affect the buffer? yes
    byte[][] banks = b.getBankData();
    harness.check(banks[0][0] == 1);
    source[0][0] = 3;
    harness.check(banks[0][0] == 3);
  
    // check null source
    boolean pass = false;
    try 
    {
      DataBufferByte b1 = new DataBufferByte((byte[][]) null, 1);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // check negative size
    DataBufferByte b1 = new DataBufferByte(source, -1);
    harness.check(b1.getSize() == -1);
  }

  private void testConstructor3(TestHarness harness)  
  {
    harness.checkPoint("DataBufferByte(byte[][], int, int[])");
    byte[][] source = new byte[][] {{1, 2}};
    DataBufferByte b = new DataBufferByte(source, 1, new int[] {0});
    harness.check(b.getSize() == 1);
    harness.check(b.getNumBanks() == 1);
    harness.check(b.getOffset() == 0);

    // test where offsets are specified
    source = new byte[][] {{1, 2, 3}, {4, 5, 6, 7}};
    b = new DataBufferByte(source, 2, new int[] {0, 1});
    harness.check(b.getSize() == 2);
    harness.check(b.getNumBanks() == 2);
    harness.check(b.getOffsets()[1] == 1);
    harness.check(b.getElem(1, 0) == 5);

    // check null source
    boolean pass = false;
    try 
    {
      DataBufferByte b1 = new DataBufferByte((byte[][]) null, 1, new int[] {0});
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
  
    // check null offsets
    pass = false;
    try 
    {
      DataBufferByte b1 = new DataBufferByte(new byte[][]{{1, 2}}, 1, (int[]) null);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
   
    // check source doesn't match offsets array
    pass = false;
    try
    {
      DataBufferByte b2 = new DataBufferByte(new byte[][]{{1, 2}}, 1, new int[] {0, 0});
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
  
  }

  private void testConstructor4(TestHarness harness) 
  {
    harness.checkPoint("DataBufferByte(byte[], int)");
    DataBufferByte b = new DataBufferByte(new byte[] {1, 2}, 2);
    harness.check(b.getSize() == 2);
    harness.check(b.getNumBanks() == 1);
    harness.check(b.getOffset() == 0);
  
    // check null source
    boolean pass = false;
    try 
    {
      // this constructor doesn't throw an exception until you
      // try to access the (null) data bank
      DataBufferByte b1 = new DataBufferByte((byte[]) null, 1);
      int ignore = b1.getElem(0);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

  private void testConstructor5(TestHarness harness) 
  {
    harness.checkPoint("DataBufferByte(byte[], int, int)");
    DataBufferByte b = new DataBufferByte(new byte[] {1, 2}, 2, 0);
    harness.check(b.getSize() == 2);
    harness.check(b.getNumBanks() == 1);
    harness.check(b.getOffset() == 0);
  
    boolean pass = false;
    try 
    {
      // this constructor doesn't throw an exception until you
      // try to access the (null) data bank
      DataBufferByte b1 = new DataBufferByte((byte[]) null, 1, 0);
      int ignore = b1.getElem(0);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // does negative size fail? no
    pass = true;
    try 
    {
      DataBufferByte b2 = new DataBufferByte(new byte[] {1, 2}, -1, 0);
    }
    catch (NegativeArraySizeException e) 
    {
      pass = false;
    }
    harness.check(pass);
    
  }

  private void testConstructor6(TestHarness harness)  
  {
    harness.checkPoint("DataBufferByte(int, int)");
    DataBufferByte b = new DataBufferByte(2, 3);
    harness.check(b.getNumBanks() == 3);
    harness.check(b.getSize() == 2);
    harness.check(b.getOffset() == 0);
  
    // does negative size fail? yes
    boolean pass = false;
    try 
    {
      DataBufferByte b1 = new DataBufferByte(-1, 1);
    }
    catch (NegativeArraySizeException e) 
    {
      pass = true;
    }
    harness.check(pass);

    // does negative banks fail? yes
    pass = false;
    try 
    {
      DataBufferByte b1 = new DataBufferByte(1, -1);
    }
    catch (NegativeArraySizeException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

}

