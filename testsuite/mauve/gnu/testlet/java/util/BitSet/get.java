/* get.java -- some checks for the get() method in the BitSet class.
   Copyright (C) 2005 David Daney
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

public class get implements Testlet
{
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
    testGeneral(harness);
  }
  
  private void test1(TestHarness harness)
  {
    harness.checkPoint("(int)");
    BitSet bs = new BitSet();
    bs.set(0);
    bs.set(2);
    harness.check(bs.get(0));
    harness.check(!bs.get(1));
    harness.check(bs.get(2));
    harness.check(!bs.get(3));
    
    boolean pass = false;
    try
    {
      bs.get(-1);
    }
    catch (IndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  private void test2(TestHarness harness)
  {
    harness.checkPoint("(int, int)");
    BitSet bs1 = new BitSet();
    bs1.set(3);
    bs1.set(4);
    bs1.set(5);
    BitSet bs2 = bs1.get(2, 5);
    harness.check(!bs2.get(0));
    harness.check(bs2.get(1));
    harness.check(bs2.get(2));
    harness.check(!bs2.get(3));
   
    BitSet bs3 = bs1.get(3, 3);
    harness.check(bs3.isEmpty());
    
    boolean pass = false;
    try
    {
      bs3 = bs1.get(-1, 1);
    }
    catch (IndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      bs3 = bs1.get(3, 1);
    }
    catch (IndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  private void testGeneral(TestHarness harness)
  {
    harness.checkPoint("testGeneral()");
    BitSet o = new BitSet(70);
    o.set(1);
    o.set(2);
    o.set(63);
    o.set(64);
      
    BitSet s1 = o.get(0, 9);
    harness.check(s1.cardinality(), 2);
    harness.check(s1.get(0), false);
    harness.check(s1.get(1), true);
    harness.check(s1.get(2), true);
    harness.check(s1.get(3), false);

    BitSet s2 = o.get(60, 69);
    harness.check(s2.cardinality(), 2);
    harness.check(s2.get(2), false);
    harness.check(s2.get(3), true);
    harness.check(s2.get(4), true);
    harness.check(s2.get(5), false);
  }
}
