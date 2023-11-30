//Tags: JDK1.2

//Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

//This file is part of Mauve.

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
//Boston, MA 02111-1307, USA.

package gnu.testlet.java.awt.image.DataBufferUShort;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.DataBufferUShort;

/**
 * Some tests for the geData() methods in the {@link DataBufferUShort} class.
 */
public class getData implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testGetData1(harness);
    testGetData2(harness);
  }

  private void testGetData1(TestHarness harness) {
    harness.checkPoint("getData()");  

    // check simple case
    short[] source = new short[] {1, 2};
    DataBufferUShort b = new DataBufferUShort(source, 2);
    short[] data = b.getData();
    harness.check(data.length == 2);
    harness.check(data[0] == 1);
    harness.check(data[1] == 2);

    // test where supplied array is bigger than 'size'
    source = new short[] {1, 2, 3};
    b = new DataBufferUShort(source, 2);
    data = b.getData();
    harness.check(data.length == 3);
    harness.check(data[0] == 1);
    harness.check(data[1] == 2);
    harness.check(data[2] == 3);

    // test where offsets are specified
    source = new short[] {1, 2, 3, 4};
    b = new DataBufferUShort(source, 2, 1);
    data = b.getData();
    harness.check(data.length == 4);
    harness.check(data[0] == 1);
    harness.check(data[1] == 2);
    harness.check(data[2] == 3);
    harness.check(data[3] == 4);

    // does a change to the source affect the DataBuffer? Yes
    source[2] = 99;
    harness.check(data[2] == 99);
  }

  private void testGetData2(TestHarness harness) {
    harness.checkPoint("getData(int)");  
  
    short[][] source = new short[][] {{1, 2}, {3, 4}};
    DataBufferUShort b = new DataBufferUShort(source, 2);
    short[] data = b.getData(1);
    harness.check(data.length == 2);
    harness.check(data[0] == 3);
    harness.check(data[1] == 4);
  
    // test where supplied array is bigger than 'size'
    source = new short[][] {{1, 2, 3}, {4, 5, 6}};
    b = new DataBufferUShort(source, 2);
    data = b.getData(1);
    harness.check(data.length == 3);
    harness.check(data[0] == 4);
    harness.check(data[1] == 5);
    harness.check(data[2] == 6);

    // test where offsets are specified
    source = new short[][] {{1, 2, 3, 4}, {5, 6, 7, 8}};
    b = new DataBufferUShort(source, 2, new int[] {1, 2});
    data = b.getData(1);
    harness.check(data.length == 4);
    harness.check(data[0] == 5);
    harness.check(data[1] == 6);
    harness.check(data[2] == 7);
    harness.check(data[3] == 8);
  
    // does a change to the source affect the DataBuffer? Yes
    source[1][2] = 99;
    harness.check(data[2] == 99);
  
    // check bounds exceptions
    boolean pass = true;
    try
    {
      b.getData(-1);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);
  
    pass = false;
    try 
    {
      b.getData(2);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

}
