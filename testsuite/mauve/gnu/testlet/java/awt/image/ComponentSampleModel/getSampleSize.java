/* getSampleSize.java -- some checks for the getSampleSize() method in the
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

import java.awt.image.ComponentSampleModel;
import java.awt.image.DataBuffer;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class getSampleSize implements Testlet
{
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
  }
  
  private void test1(TestHarness harness)
  {
    harness.checkPoint("()");  
    ComponentSampleModel m1 = new ComponentSampleModel(DataBuffer.TYPE_INT, 22, 
            11, 2, 44, new int[] {0, 0});
    int[] sizes = m1.getSampleSize();
    harness.check(sizes.length, 2);
    harness.check(sizes[0], 32);
    harness.check(sizes[1], 32);

    ComponentSampleModel m2 = new ComponentSampleModel(DataBuffer.TYPE_BYTE, 22, 
            11, 2, 44, new int[] {0, 0, 0});
    int[] sizes2 = m2.getSampleSize();
    harness.check(sizes2.length, 3);
    harness.check(sizes2[0], 8);
    harness.check(sizes2[1], 8);
    harness.check(sizes2[2], 8);
  }
  
  private void test2(TestHarness harness)
  {
    harness.checkPoint("(int)");  
    ComponentSampleModel m1 = new ComponentSampleModel(DataBuffer.TYPE_INT, 22, 
            11, 2, 44, new int[] {0, 0});
    harness.check(m1.getSampleSize(0), 32);
    harness.check(m1.getSampleSize(1), 32);
    harness.check(m1.getSampleSize(2), 32);
    harness.check(m1.getSampleSize(3), 32);
    harness.check(m1.getSampleSize(-1), 32);
  }
}
