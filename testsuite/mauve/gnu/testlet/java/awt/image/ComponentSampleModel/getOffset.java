/* getOffset.java -- some checks for the getOffset() methods in the 
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

public class getOffset implements Testlet
{
  public void test(TestHarness harness)
  {
    testMethod1(harness);
    testMethod2(harness);
  }
  
  public void testMethod1(TestHarness harness)
  {
    harness.checkPoint("(int, int)");
    ComponentSampleModel m = new ComponentSampleModel(DataBuffer.TYPE_BYTE, 5, 
            10, 3, 15, new int[] {0, 1, 2});
    harness.check(m.getOffset(0, 0), 0);
    harness.check(m.getOffset(1, 0), 3);
    harness.check(m.getOffset(2, 0), 6);
    harness.check(m.getOffset(3, 0), 9);
    harness.check(m.getOffset(4, 0), 12);
    harness.check(m.getOffset(5, 0), 15);
    harness.check(m.getOffset(0, 1), 15);
    harness.check(m.getOffset(1, 1), 18);
    harness.check(m.getOffset(2, 1), 21);
    harness.check(m.getOffset(3, 1), 24);
    harness.check(m.getOffset(4, 1), 27);
    harness.check(m.getOffset(5, 1), 30);
  }

  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(int, int, int)");
    ComponentSampleModel m = new ComponentSampleModel(DataBuffer.TYPE_BYTE, 5, 
            10, 3, 15, new int[] {0, 1, 2});
    harness.check(m.getOffset(0, 0, 1), 1);
    harness.check(m.getOffset(1, 0, 1), 4);
    harness.check(m.getOffset(2, 0, 1), 7);
    harness.check(m.getOffset(3, 0, 1), 10);
    harness.check(m.getOffset(4, 0, 1), 13);
    harness.check(m.getOffset(5, 0, 1), 16);
    harness.check(m.getOffset(0, 1, 1), 16);
    harness.check(m.getOffset(1, 1, 1), 19);
    harness.check(m.getOffset(2, 1, 1), 22);
    harness.check(m.getOffset(3, 1, 1), 25);
    harness.check(m.getOffset(4, 1, 1), 28);
    harness.check(m.getOffset(5, 1, 1), 31);
  }
}
