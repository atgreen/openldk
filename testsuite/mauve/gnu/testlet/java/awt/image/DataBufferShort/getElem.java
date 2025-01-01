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

package gnu.testlet.java.awt.image.DataBufferShort;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.DataBuffer;
import java.awt.image.DataBufferShort;


/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class getElem
  implements Testlet
{
  public void test(TestHarness h)
  {
    DataBuffer buf;
    short[] data = new short[] { -11, -22, -33, -44 };
    
    buf = new DataBufferShort(new short[][] { data, data }, 2,
                              new int[] { 2, 0 });

    h.check(buf.getElem(0), -33);    // Check #1.
    h.check(buf.getElem(1), -44);    // Check #2.
    h.check(buf.getElem(0, 0), -33); // Check #3.
    h.check(buf.getElem(0, 1), -44); // Check #4.
    h.check(buf.getElem(1, 0), -11); // Check #5.
    h.check(buf.getElem(1, 1), -22); // Check #6.
    
    // new tests added by David Gilbert
    testGetElem1(h);
    testGetElem2(h);
    
  }
  
  private void testGetElem1(TestHarness harness) 
  {
    harness.checkPoint("getElem(int)");  
      
    // test where supplied array is bigger than 'size'
    short[] source = new short[] {1, 2, 3};
    DataBufferShort b = new DataBufferShort(source, 2);
    harness.check(b.getElem(0) == 1);
    harness.check(b.getElem(1) == 2);
    harness.check(b.getElem(2) == 3);
    
    boolean pass = false;
    try
    {
      b.getElem(-1);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      b.getElem(3);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // test where offsets are specified
    source = new short[] {1, 2, 3, 4};
    b = new DataBufferShort(source, 2, 1);
    harness.check(b.getElem(-1) == 1);
    harness.check(b.getElem(0) == 2);
    harness.check(b.getElem(1) == 3);
    harness.check(b.getElem(2) == 4);

    pass = false;
    try
    {
      b.getElem(3);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);

    pass = false;
    try
    {
      b.getElem(-2);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

  private void testGetElem2(TestHarness harness) {
    harness.checkPoint("getElem(int, int)");  
    
    short[][] source = new short[][] {{1, 2}, {3, 4}};
    DataBufferShort b = new DataBufferShort(source, 2);
    harness.check(b.getElem(1, 0) == 3);
    harness.check(b.getElem(1, 1) == 4);
    
    // test where supplied array is bigger than 'size'
    source = new short[][] {{1, 2, 3}, {4, 5, 6}};
    b = new DataBufferShort(source, 2);
    harness.check(b.getElem(1, 2) == 6);
      
    // test where offsets are specified
    source = new short[][] {{1, 2, 3, 4}, {5, 6, 7, 8}};
    b = new DataBufferShort(source, 2, new int[] {1, 2});
    harness.check(b.getElem(1, -2) == 5);
    harness.check(b.getElem(1, -1) == 6);
    harness.check(b.getElem(1, 0) == 7);
    harness.check(b.getElem(1, 1) == 8);
       
    // does a change to the source affect the DataBuffer? Yes
    source[1][2] = 99;
    harness.check(source[1][2] == 99);
    harness.check(b.getElem(1, 0) == 99);
        
    // test when the bank index is out of bounds
    boolean pass = true;
    try
    {
      b.getElem(-1, 0);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);
        
    pass = false;
    try 
    {
      b.getElem(2, 0);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // test when the item index is out of bounds
    pass = true;
    try
    {
      b.getElem(0, -2);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);
        
    pass = false;
    try 
    {
      b.getElem(1, 5);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // the array of arrays should reflect the single dimension array
    DataBufferShort b2 = new DataBufferShort(new short[] {1, 2, 3}, 3);
    harness.check(b2.getElem(0, 1) == 2);
  }
}
