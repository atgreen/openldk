/* setPixel.java -- some checks for the setPixel() method in the
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

public class setPixel implements Testlet
{
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
  }
    
  public void test1(TestHarness harness)
  {
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_INT, 10, 20, 8);
    DataBuffer db = m.createDataBuffer();
    m.setPixel(0, 1, new int[] {0xAA}, db);
    m.setPixel(1, 1, new int[] {0xBB}, db);
    m.setPixel(2, 1, new int[] {0xCC}, db);
    m.setPixel(3, 1, new int[] {0xDD}, db);
    harness.check(db.getElem(3), 0xAABBCCDD);
      
    // try null db
    boolean pass = false;
    try 
    {
      m.setPixel(3, 1, new int[] {0xAA}, null);
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
    m.setPixel(2, 1, new int[] {0xAA}, db);
    m.setPixel(3, 1, new int[] {0xBB}, db);
    m.setPixel(4, 1, new int[] {0xCC}, db);
    m.setPixel(5, 1, new int[] {0xDD}, db);
    harness.check(db.getElem(5), 0xAABBCCDD);
  }

}
