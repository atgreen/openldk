/* createCompatibleSampleModel.java -- some checks for the 
       createCompatibleSampleModel() method in the ComponentSampleModel class.
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
import java.awt.image.SampleModel;
import java.util.Arrays;

public class createCompatibleSampleModel implements Testlet
{
  public void test(TestHarness harness)
  {
    ComponentSampleModel m1 = new ComponentSampleModel(DataBuffer.TYPE_INT, 22, 
            11, 1, 25, new int[] {1, 2}, new int[] {3, 4});
    SampleModel m2 = m1.createCompatibleSampleModel(33, 44);
    harness.check(m2 instanceof ComponentSampleModel);
    harness.check(m2.getDataType(), DataBuffer.TYPE_INT);
    harness.check(m2.getWidth(), 33);
    harness.check(m2.getHeight(), 44);
    harness.check(m2.getNumBands(), 2);
    harness.check(Arrays.equals(m1.getBandOffsets(), new int[] {3, 4}));
    harness.check(Arrays.equals(m1.getBankIndices(), new int[] {1, 2}));
  }
}
