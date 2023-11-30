/* getBitOffset.java -- some checks for the getBitOffset() method in the
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

public class getBitOffset implements Testlet
{
  public void test(TestHarness harness)
  {
    MultiPixelPackedSampleModel m = new MultiPixelPackedSampleModel(
        DataBuffer.TYPE_INT, 10, 20, 8);
    harness.check(m.getBitOffset(0), 0);
    harness.check(m.getBitOffset(1), 8);
    harness.check(m.getBitOffset(2), 16);
    harness.check(m.getBitOffset(3), 24);
    harness.check(m.getBitOffset(4), 0);
    harness.check(m.getBitOffset(5), 8);
    harness.check(m.getBitOffset(6), 16);
    harness.check(m.getBitOffset(7), 24);
    harness.check(m.getBitOffset(8), 0);
    harness.check(m.getBitOffset(9), 8);
    harness.check(m.getBitOffset(-1), -8);
    
    MultiPixelPackedSampleModel m2 = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_INT, 10, 20, 8, 4, 16);
    harness.check(m2.getBitOffset(0), 16);
    harness.check(m2.getBitOffset(1), 24);
    harness.check(m2.getBitOffset(2), 0);
    harness.check(m2.getBitOffset(3), 8);
    harness.check(m2.getBitOffset(4), 16);
    harness.check(m2.getBitOffset(5), 24);
    harness.check(m2.getBitOffset(6), 0);
    harness.check(m2.getBitOffset(7), 8);
    harness.check(m2.getBitOffset(8), 16);
    harness.check(m2.getBitOffset(9), 24);
    harness.check(m2.getBitOffset(-1), 8);

    MultiPixelPackedSampleModel m3 = new MultiPixelPackedSampleModel(
            DataBuffer.TYPE_INT, 10, 20, 16);
    harness.check(m3.getBitOffset(0), 0);
    harness.check(m3.getBitOffset(1), 16);
    harness.check(m3.getBitOffset(2), 0);
    harness.check(m3.getBitOffset(3), 16);
    harness.check(m3.getBitOffset(4), 0);
    harness.check(m3.getBitOffset(5), 16);
    harness.check(m3.getBitOffset(6), 0);
    harness.check(m3.getBitOffset(7), 16);
    harness.check(m3.getBitOffset(8), 0);
    harness.check(m3.getBitOffset(9), 16);
    harness.check(m3.getBitOffset(-1), -16);
  }
}
