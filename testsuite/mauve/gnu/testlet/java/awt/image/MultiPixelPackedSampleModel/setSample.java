/* setSample.java -- some checks for the setSample() method in the
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

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.DataBuffer;
import java.awt.image.MultiPixelPackedSampleModel;

public class setSample implements Testlet
{
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
    testTYPE_USHORT(harness);
    testTYPE_BYTE(harness);
  }
    
  public void test1(TestHarness harness)
  {
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_INT, 10, 20, 8);
    DataBuffer db = m.createDataBuffer();
    m.setSample(0, 1, 0, 0xAA, db);
    m.setSample(1, 1, 0, 0xBB, db);
    m.setSample(2, 1, 0, 0xCC, db);
    m.setSample(3, 1, 0, 0xDD, db);
    harness.check(db.getElem(3), 0xAABBCCDD);
      
    // try null db
    boolean pass = false;
    try 
    {
      m.setSample(3, 1, 0, 0xAA, null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
    
  public void test2(TestHarness harness)
  {
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
              DataBuffer.TYPE_INT, 10, 20, 8, 4, 16);
    DataBuffer db = m.createDataBuffer();
    m.setSample(2, 1, 0, 0xAA, db);
    m.setSample(3, 1, 0, 0xBB, db);
    m.setSample(4, 1, 0, 0xCC, db);
    m.setSample(5, 1, 0, 0xDD, db);
    harness.check(db.getElem(5), 0xAABBCCDD);
  }
  
  public void testTYPE_USHORT(TestHarness harness)
  {
    harness.checkPoint("TYPE_USHORT");
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_USHORT, 10, 20, 8);
    DataBuffer db = m.createDataBuffer();
    m.setSample(0, 0, 0, 0x12, db);
    harness.check(db.getElem(0), 0x1200);
    m.setSample(1, 0, 0, 0x34, db);
    harness.check(db.getElem(0), 0x1234);
    m.setSample(2, 1, 0, 0xAB, db);
    harness.check(db.getElem(6), 0xAB00);
    m.setSample(3, 1, 0, 0xCD, db);
    harness.check(db.getElem(6), 0xABCD);
  }

  public void testTYPE_BYTE(TestHarness harness)
  {
    harness.checkPoint("TYPE_BYTE");
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_BYTE, 10, 20, 2);
    DataBuffer db = m.createDataBuffer();
    m.setSample(0, 0, 0, 0x01, db);
    harness.check(db.getElem(0), 0x40);
    m.setSample(1, 0, 0, 0x02, db);
    harness.check(db.getElem(0), 0x60);
    m.setSample(2, 1, 0, 0x03, db);
    harness.check(db.getElem(3), 0x0C);
    m.setSample(3, 1, 0, 0x04, db);
    harness.check(db.getElem(3), 0x0C);
  }
}
