/* clear.java -- some checks for the clear() methods in the BitSet class.
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

package gnu.testlet.java.util.BitSet;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.BitSet;

public class clear implements Testlet
{
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
    test3(harness);
  }
  
  private void test1(TestHarness harness)
  {
    harness.checkPoint("()");
    BitSet bs = new BitSet(8);
    bs.set(1);
    bs.clear();
    harness.check(bs.isEmpty());
  }
  
  private void test2(TestHarness harness)
  {
    harness.checkPoint("(int)");
    BitSet bs = new BitSet(7);
    bs.set(6);
    bs.clear(6);
    harness.check(bs.isEmpty());
    
    boolean pass = false;
    try
    {
      bs.clear(-1);
    }
    catch (IndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    bs.clear(7);
    harness.check(bs.nextClearBit(7), 7);
  }
  
  private void test3(TestHarness harness)
  {
    harness.checkPoint("(int, int)");
    BitSet bs = new BitSet(9);
    bs.set(3);
    bs.set(4);
    bs.set(5);
    bs.clear(2, 4);
    harness.check(!bs.get(3));
    harness.check(bs.get(4));
    harness.check(bs.get(5));
    
    bs.clear(5, 5);
    harness.check(bs.get(5));

    boolean pass = false;
    try
    {
      bs.clear(-1, 2);
    }
    catch (IndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      bs.clear(2, 1);
    }
    catch (IndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
