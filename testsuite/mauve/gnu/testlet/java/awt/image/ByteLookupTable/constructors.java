/* constructors.java -- some checks for the constructors in the ByteLookupTable
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

package gnu.testlet.java.awt.image.ByteLookupTable;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.ByteLookupTable;

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
    byte[] bytes = new byte[] {(byte) 0xAA, (byte) 0xBB};
    ByteLookupTable t = new ByteLookupTable(7, bytes);
    harness.check(t.getOffset(), 7);
    harness.check(t.getNumComponents(), 1);
    byte[][] table = t.getTable();
    harness.check(table.length, 1);
    harness.check(table[0] == bytes);
    
    // try negative offset
    boolean pass = false;
    try
    {
      t = new ByteLookupTable(-1, bytes);
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
      t = new ByteLookupTable(0, (byte[]) null);
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
    byte[] bytesA = new byte[] {(byte) 0xAA, (byte) 0xBB, (byte) 0xCC};
    byte[] bytesB = new byte[] {(byte) 0xDD, (byte) 0xEE, (byte) 0xFF};
    byte[][] bytes = new byte[][] { bytesA, bytesB };
    ByteLookupTable t = new ByteLookupTable(3, bytes);
    harness.check(t.getOffset(), 3);
    harness.check(t.getNumComponents(), 2);
    byte[][] table = t.getTable();
    harness.check(table.length, 2);
    harness.check(table[0] == bytesA);
    harness.check(table[1] == bytesB);
    harness.check(table != bytes);
    
    // try negative offset
    boolean pass = false;
    try
    {
      t = new ByteLookupTable(-1, bytes);
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
      t = new ByteLookupTable(0, (byte[][]) null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
