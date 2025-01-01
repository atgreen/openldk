/* getRenderingHints.java -- some checks for the getRenderingHints() method 
       in the ConvolveOp class.
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

package gnu.testlet.java.awt.image.ConvolveOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.RenderingHints;
import java.awt.image.ConvolveOp;
import java.awt.image.Kernel;

public class getRenderingHints implements Testlet
{
  public void test(TestHarness harness)
  {
    Kernel k1 = new Kernel(3, 3, new float[] {1f, 2f, 3f, 4f, 5f, 6f, 7f, 8f, 
            9f});
    RenderingHints r = new RenderingHints(RenderingHints.KEY_DITHERING, 
            RenderingHints.VALUE_DITHER_DISABLE);
    ConvolveOp op = new ConvolveOp(k1, ConvolveOp.EDGE_NO_OP, r);
    harness.check(op.getRenderingHints(), r);
    harness.check(op.getRenderingHints().size(), 1);
    op = new ConvolveOp(k1, ConvolveOp.EDGE_NO_OP, null);
    harness.check(op.getRenderingHints(), null);
  }
}
