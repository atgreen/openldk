/* getSampleDouble.java -- some checks for the getSampleDouble() method in the
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

public class getSampleDouble implements Testlet
{
  public void test(TestHarness harness)
  {
    ComponentSampleModel m = new ComponentSampleModel(DataBuffer.TYPE_BYTE, 5, 
            10, 3, 16, new int[] {0, 1, 2});
    DataBuffer db = m.createDataBuffer();
    harness.check(db.getNumBanks(), 1);
    db.setElem(0, 16, 0xAA);
    db.setElem(0, 17, 0xBB);
    db.setElem(0, 18, 0xCC);
    harness.check(m.getSampleDouble(0, 1, 0, db), 0xAA);
    harness.check(m.getSampleDouble(0, 1, 1, db), 0xBB);
    harness.check(m.getSampleDouble(0, 1, 2, db), 0xCC);
    
    // try negative x
    boolean pass = false;
    try
    {
      m.getSampleDouble(-1, 1, 0, db);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);

    // try x == width
    pass = false;
    try
    {
      m.getSampleDouble(5, 0, 0, db);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
  
    // try negative y
    pass = false;
    try
    {
      m.getSampleDouble(1, -1, 0, db);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
  
    // try y == height
    pass = false;
    try
    {
      m.getSampleDouble(0, 10, 0, db);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
