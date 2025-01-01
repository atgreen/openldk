/* hashCode.java -- some checks for the hashCode() method in the
       MultiPixelPackedSampleModel class.
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

package gnu.testlet.java.awt.image.MultiPixelPackedSampleModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.DataBuffer;
import java.awt.image.MultiPixelPackedSampleModel;

public class hashCode implements Testlet
{
  public void test(TestHarness harness)
  {
    // equal instances should have equal hashCodes...
    MultiPixelPackedSampleModel m1 = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_INT, 10, 20, 8, 5, 1);
    MultiPixelPackedSampleModel m2 = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_INT, 10, 20, 8, 5, 1);
    harness.check(m1.equals(m2));
    harness.check(m1.hashCode(), m2.hashCode());
  }
}
