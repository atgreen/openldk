/* constructors.java -- some checks for the constructors in the ShortLookupTable
       class.
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

package gnu.testlet.java.awt.image.ShortLookupTable;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.ShortLookupTable;

public class constructors implements Testlet
{
  public void test(TestHarness harness)
  {
    testConstructor1(harness);
    testConstructor2(harness);
  }
  
  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("(int, byte[])");
    short[] shorts = new short[] {0xAA, 0xBB};
    ShortLookupTable t = new ShortLookupTable(7, shorts);
    harness.check(t.getOffset(), 7);
    harness.check(t.getNumComponents(), 1);
    short[][] table = t.getTable();
    harness.check(table.length, 1);
    harness.check(table[0] == shorts);
    
    // try negative offset
    boolean pass = false;
    try
    {
      t = new ShortLookupTable(-1, shorts);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try null data
    pass = false;
    try
    {
      t = new ShortLookupTable(0, (short[]) null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  public void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(int, byte[][])");
    short[] shortsA = new short[] {0xAA, 0xBB, 0xCC};
    short[] shortsB = new short[] {0xDD, 0xEE, 0xFF};
    short[][] shorts = new short[][] { shortsA, shortsB };
    ShortLookupTable t = new ShortLookupTable(3, shorts);
    harness.check(t.getOffset(), 3);
    harness.check(t.getNumComponents(), 2);
    short[][] table = t.getTable();
    harness.check(table.length, 2);
    harness.check(table[0] == shortsA);
    harness.check(table[1] == shortsB);
    harness.check(table != shorts);
    
    // try negative offset
    boolean pass = false;
    try
    {
      t = new ShortLookupTable(-1, shorts);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try null data
    pass = false;
    try
    {
      t = new ShortLookupTable(0, (short[][]) null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
