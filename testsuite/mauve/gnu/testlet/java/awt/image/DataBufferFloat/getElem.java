// Tags: JDK1.4

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

package gnu.testlet.java.awt.image.DataBufferFloat;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.image.DataBuffer;
import java.awt.image.DataBufferFloat;


/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class getElem
  implements Testlet
{
  public void test(TestHarness h)
  {
    DataBuffer buf;
    float[] data = new float[] { 1.1f, -2.2f, 3.3f, -4.4f };
    
    buf = new DataBufferFloat(new float[][] { data, data }, 2,
                              new int[] { 2, 0 });

    h.check(buf.getElem(0), 3);     // Check #1.
    h.check(buf.getElem(1), -4);    // Check #2.
    h.check(buf.getElem(0, 0), 3);  // Check #3.
    h.check(buf.getElem(0, 1), -4); // Check #4.
    h.check(buf.getElem(1, 0), 1);  // Check #5.
    h.check(buf.getElem(1, 1), -2); // Check #6.
    // new tests added by David Gilbert
    testGetElem1(h);
    testGetElem2(h);
    
  }
  
  private void testGetElem1(TestHarness harness) 
  {
    harness.checkPoint("getElem(int)");  
      
    // test where supplied array is bigger than 'size'
    float[] source = new float[] {1, 2, 3};
    DataBufferFloat b = new DataBufferFloat(source, 2);
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
    source = new float[] {1, 2, 3, 4};
    b = new DataBufferFloat(source, 2, 1);
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
    
    float[][] source = new float[][] {{1, 2}, {3, 4}};
    DataBufferFloat b = new DataBufferFloat(source, 2);
    harness.check(b.getElem(1, 0) == 3);
    harness.check(b.getElem(1, 1) == 4);
    
    // test where supplied array is bigger than 'size'
    source = new float[][] {{1, 2, 3}, {4, 5, 6}};
    b = new DataBufferFloat(source, 2);
    harness.check(b.getElem(1, 2) == 6);
      
    // test where offsets are specified
    source = new float[][] {{1, 2, 3, 4}, {5, 6, 7, 8}};
    b = new DataBufferFloat(source, 2, new int[] {1, 2});
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
    DataBufferFloat b2 = new DataBufferFloat(new float[] {1, 2, 3}, 3);
    harness.check(b2.getElem(0, 1) == 2);
  }
}
