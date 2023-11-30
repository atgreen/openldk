/* getRenderingHints.java -- some checks for the getRenderingHints() method
       in the RescaleOp class.
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

package gnu.testlet.java.awt.image.RescaleOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.RenderingHints;
import java.awt.image.RescaleOp;

public class getRenderingHints implements Testlet 
{
  public void test(TestHarness harness)
  {
    RenderingHints r = new RenderingHints(RenderingHints.KEY_COLOR_RENDERING,
            RenderingHints.VALUE_COLOR_RENDER_QUALITY);
    RescaleOp op = new RescaleOp(1, 1, r);
    harness.check(op.getRenderingHints() == r);
    op = new RescaleOp(1, 1, null);
    harness.check(op.getRenderingHints() == null);
  }
}
