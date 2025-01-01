/* setSample.java -- some checks for the setSample() method in the
       ComponentSampleModel class.
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

package gnu.testlet.java.awt.image.ComponentSampleModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.ComponentSampleModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.DataBufferDouble;
import java.awt.image.DataBufferFloat;
import java.awt.image.DataBufferInt;

public class setSample implements Testlet
{
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
    test3(harness);
  }
  
  private void test1(TestHarness harness)
  {
    harness.checkPoint("(int, int, int, double, DataBuffer)");  
    DataBuffer db = new DataBufferDouble(12);
    ComponentSampleModel m = new ComponentSampleModel(DataBuffer.TYPE_DOUBLE,
            3, 2, 2, 6, new int[] {0, 1});
    m.setSample(2, 1, 0, 99.9, db);
    harness.check(db.getElem(0, 10), 99.0d);
    m.setSample(2, 1, 1, 88.8, db);
    harness.check(db.getElem(0, 11), 88.0d);
    
    // what happens if the data buffer doesn't hold doubles?
    DataBuffer db2 = new DataBufferInt(12);
    m.setSample(2, 1, 0, 99.9d, db2);
    harness.check(db2.getElem(0, 10), 99);
    m.setSample(2, 1, 1, 88.8d, db2);
    harness.check(db2.getElem(0, 11), 88); 

    // check that a null data buffer generates a NullPointerException
    boolean pass = false;
    try
      {
        m.setSample(2, 1, 1, 77.7d, null);
      }
    catch (NullPointerException e)
      {
        pass = true;
      }
    harness.check(pass);
  }
  
  private void test2(TestHarness harness)
  {
    harness.checkPoint("(int, int, int, float, DataBuffer)");  

    DataBuffer db = new DataBufferFloat(12);
    ComponentSampleModel m = new ComponentSampleModel(DataBuffer.TYPE_FLOAT,
            3, 2, 2, 6, new int[] {0, 1});
    m.setSample(2, 1, 0, 99.9f, db);
    harness.check(db.getElem(0, 10), 99.0f);
    m.setSample(2, 1, 1, 88.8f, db);
    harness.check(db.getElem(0, 11), 88.0f);
    
    // what happens if the data buffer doesn't hold floats?
    DataBuffer db2 = new DataBufferInt(12);
    m.setSample(2, 1, 0, 99.9f, db2);
    harness.check(db2.getElem(0, 10), 99);
    m.setSample(2, 1, 1, 88.8f, db2);
    harness.check(db2.getElem(0, 11), 88); 
     
    // check that a null data buffer generates a NullPointerException
    boolean pass = false;
    try
      {
        m.setSample(2, 1, 1, 77.7f, null);
      }
    catch (NullPointerException e)
      {
        pass = true;
      }
    harness.check(pass);
  }
  
  private void test3(TestHarness harness)
  {
    harness.checkPoint("(int, int, int, int, DataBuffer)");  
    
    DataBuffer db = new DataBufferInt(12);
    ComponentSampleModel m = new ComponentSampleModel(DataBuffer.TYPE_INT,
            3, 2, 2, 6, new int[] {0, 1});
    m.setSample(2, 1, 0, 99, db);
    harness.check(db.getElem(0, 10), 99);
    m.setSample(2, 1, 1, 88, db);
    harness.check(db.getElem(0, 11), 88);
    
    // what happens if the data buffer doesn't hold integers?
    DataBuffer db2 = new DataBufferByte(12);
    m.setSample(2, 1, 0, 99, db2);
    harness.check(db2.getElem(0, 10), 99);
    m.setSample(2, 1, 1, 888, db2);
    harness.check(db2.getElem(0, 11), 120);  // (byte) 888
    
    // check that a null data buffer generates a NullPointerException
    boolean pass = false;
    try
      {
        m.setSample(2, 1, 1, 77, null);
      }
    catch (NullPointerException e)
      {
        pass = true;
      }
    harness.check(pass);
  }
}
