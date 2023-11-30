/* getBandOffsets.java -- some checks for the getBandOffsets() method in the
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
import java.util.Arrays;

public class getBandOffsets implements Testlet
{
  public void test(TestHarness harness)
  {
    int[] bo = new int[] {1, 2};
    ComponentSampleModel m1 = new ComponentSampleModel(DataBuffer.TYPE_INT, 22, 
            33, 1, 23, bo);
    int[] bo1 = m1.getBandOffsets();
    harness.check(Arrays.equals(bo1, new int[] {1, 2}));
    int[] bo2 = m1.getBandOffsets();
    harness.check(bo1 != bo2);
    bo[1] = 3;
    harness.check(m1.getBandOffsets()[1], 2);
  }
}
