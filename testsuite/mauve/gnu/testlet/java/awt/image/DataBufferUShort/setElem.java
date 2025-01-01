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
 * Some tests for the setElem() methods in the {@link DataBufferUShort} class.
 */
public class setElem implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testSetElem1(harness);
    testSetElem2(harness); 
  }

  private void testSetElem1(TestHarness harness) {
    harness.checkPoint("setElem(int, int)");   

    short[] source = new short[] {1, 2};
    DataBufferUShort b = new DataBufferUShort(source, 2);
    b.setElem(1, 99);
    harness.check(b.getElem(1) == 99);

    // does the source array get updated? Yes
    harness.check(source[1] == 99);

    // test with offsets
    source = new short[] {1, 2, 3, 4, 5};
    b = new DataBufferUShort(source, 4, 1);
    harness.check(b.getElem(1) == 3);
    b.setElem(1, 99);
    harness.check(b.getElem(1) == 99); 
    harness.check(source[2] == 99);

    boolean pass = false;
    try
    {
      b.setElem(-2, 99);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);

    pass = false;
    try
    {
      b.setElem(4, 99);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

  private void testSetElem2(TestHarness harness) {
    harness.checkPoint("setElem(int, int, int)");  

    short[][] source = new short[][] {{1, 2}, {3, 4}};
    DataBufferUShort b = new DataBufferUShort(source, 2);
    b.setElem(1, 1, 99);
    harness.check(b.getElem(1, 1) == 99);
    // does the source array get updated?
    harness.check(source[1][1] == 99);

    boolean pass = false;
    try
    {
      b.setElem(-1, 1, 99);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);
  
    pass = false;
    try
    {
      b.setElem(2, 1, 99);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }
}
