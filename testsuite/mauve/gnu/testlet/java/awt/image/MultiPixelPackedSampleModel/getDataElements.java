/* getDataElements.java -- some checks for the getDataElements() method in the
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

public class getDataElements implements Testlet
{
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
  }
  
  public void test1(TestHarness harness)
  {
    harness.checkPoint("test1()");
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_INT, 10, 20, 8);
    DataBuffer db = m.createDataBuffer();
    db.setElem(3, 0xAABBCCDD);
    
    byte[] elements = (byte[]) m.getDataElements(0, 1, null, db);
    harness.check(elements.length, 1);
    harness.check(elements[0], (byte) 0xAA);

    elements = (byte[]) m.getDataElements(1, 1, null, db);
    harness.check(elements.length, 1);
    harness.check(elements[0], (byte) 0xBB);

    elements = (byte[]) m.getDataElements(2, 1, null, db);
    harness.check(elements.length, 1);
    harness.check(elements[0], (byte) 0xCC);

    elements = (byte[]) m.getDataElements(3, 1, null, db);
    harness.check(elements.length, 1);
    harness.check(elements[0], (byte) 0xDD);
    
    // try null db
    boolean pass = false;
    try 
    {
      m.getDataElements(3, 1, null, null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  public void test2(TestHarness harness)
  {
    harness.checkPoint("test2()");
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_INT, 10, 20, 16);
    DataBuffer db = m.createDataBuffer();
    db.setElem(5, 0xAABBCCDD);
    
    short[] elements = (short[]) m.getDataElements(0, 1, null, db);
    harness.check(elements.length, 1);
    harness.check(elements[0], (short) 0xAABB);

    elements = (short[]) m.getDataElements(1, 1, null, db);
    harness.check(elements.length, 1);
    harness.check(elements[0], (short) 0xCCDD);
  }

}
