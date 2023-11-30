/* flip.java -- some checks for the flip() method in the BitSet clas.
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

public class flip implements Testlet
{
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
  }
  
  public void test1(TestHarness harness)
  {
    harness.checkPoint("(int)");
    BitSet bs = new BitSet(17);
    bs.flip(11);
    harness.check(bs.nextSetBit(0), 11);
    
    boolean pass = false;
    try
    {
      bs.flip(-1);
    }
    catch (IndexOutOfBoundsException e)
    { 
      pass = true;
    }
    harness.check(pass);
  }
  
  public void test2(TestHarness harness)
  {
    harness.checkPoint("(int, int)");
    BitSet bs = new BitSet(21);
    bs.flip(3, 5);
    harness.check(!bs.get(2));
    harness.check(bs.get(3));
    harness.check(bs.get(4));
    harness.check(!bs.get(5));
    
    bs.flip(4, 4);
    harness.check(bs.get(4));
    
    boolean pass = false;
    try
    {
      bs.flip(-1, 1);
    }
    catch (IndexOutOfBoundsException e)
    { 
      pass = true;
    }
    harness.check(pass);

    pass = false;
    try
    {
      bs.flip(2, 1);
    }
    catch (IndexOutOfBoundsException e)
    { 
      pass = true;
    }
    harness.check(pass);
  }
}
