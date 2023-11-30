/* createDataBuffer.java -- some checks for the createDataBuffer() method in 
       the ComponentSampleModel class.
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

public class createDataBuffer implements Testlet
{
  public void test(TestHarness harness)
  {
    testSingleBand(harness);
    testMultiBand(harness);
  }
  
  public void testSingleBand(TestHarness harness)
  {
    ComponentSampleModel m = new ComponentSampleModel(DataBuffer.TYPE_BYTE, 10, 
            20, 1, 30, new int[] {0, 1, 2});
    DataBuffer db = m.createDataBuffer();
    harness.check(db.getDataType(), DataBuffer.TYPE_BYTE);
    harness.check(db.getNumBanks(), 1);
    harness.check(db.getSize(), 582);
    
    m = new ComponentSampleModel(DataBuffer.TYPE_INT, 5, 10, 1, 5, 
            new int[] {0, 1, 2});
    db = m.createDataBuffer();
    harness.check(db.getDataType(), DataBuffer.TYPE_INT);
    harness.check(db.getNumBanks(), 1);
    harness.check(db.getSize(), 52);

    m = new ComponentSampleModel(DataBuffer.TYPE_INT, 5, 10, 1, 6, 
            new int[] {0, 1, 2});
    db = m.createDataBuffer();
    harness.check(db.getDataType(), DataBuffer.TYPE_INT);
    harness.check(db.getNumBanks(), 1);
    harness.check(db.getSize(), 61);  

    m = new ComponentSampleModel(DataBuffer.TYPE_INT, 5, 10, 2, 10, 
            new int[] {0, 1});
    db = m.createDataBuffer();
    harness.check(db.getDataType(), DataBuffer.TYPE_INT);
    harness.check(db.getNumBanks(), 1);
    harness.check(db.getSize(), 100);    
  }
  
  public void testMultiBand(TestHarness harness)
  {
    harness.checkPoint("testMultiBand()");
    ComponentSampleModel m = new ComponentSampleModel(DataBuffer.TYPE_BYTE, 10, 
            20, 1, 10, new int[] {0, 1, 2}, new int[] {0, 0, 0});
    DataBuffer db = m.createDataBuffer();
    harness.check(db.getDataType(), DataBuffer.TYPE_BYTE);
    harness.check(db.getNumBanks(), 3);
    harness.check(db.getSize(), 200);
    
  }
}
