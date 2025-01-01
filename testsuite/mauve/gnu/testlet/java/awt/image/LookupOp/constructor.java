/* constructor.java -- some checks for the constructor in the LookupOp class.
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

package gnu.testlet.java.awt.image.LookupOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.RenderingHints;
import java.awt.image.ByteLookupTable;
import java.awt.image.LookupOp;

public class constructor implements Testlet
{
  public void test(TestHarness harness)
  {
    byte[] bytes = new byte[] {(byte) 0xAA, (byte) 0xBB};
    ByteLookupTable t = new ByteLookupTable(0, bytes);
    RenderingHints r = new RenderingHints(RenderingHints.KEY_COLOR_RENDERING,
            RenderingHints.VALUE_COLOR_RENDER_QUALITY);
    LookupOp op = new LookupOp(t, r);
    harness.check(op.getTable() == t);
    harness.check(op.getRenderingHints() == r);

    // try null RenderingHints
    op = new LookupOp(t, null);
    harness.check(op.getRenderingHints(), null);
  }
}
