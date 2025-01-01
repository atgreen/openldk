// Tags: JDK1.2
// Uses: TestLayout

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version. 

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.JComponent;

import javax.swing.JComponent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class getAlignmentX implements Testlet
{

  public void test(TestHarness harness)
  {
    JComponent c = new JComponent(){};
    TestLayout l = new TestLayout();
    c.setLayout(l);
    l.alignmentX = 0.3F;
    // No alignment has been set, so the layout alignment is returned.
    harness.check(c.getAlignmentX(), 0.3F);
    // Now we set the alignment to a valid value and this should be returned.
    c.setAlignmentX(0.2F);
    harness.check(c.getAlignmentX(), 0.2F);
    // Now we set the alignment to something great, and the component
    // should return the nearest valid value (1.0 in this case).
    c.setAlignmentX(100.0F);
    harness.check(c.getAlignmentX(), 1.0F);
    // Now we set the alignment to something negative.
    c.setAlignmentX(-100.0F);
    harness.check(c.getAlignmentX(), 0.0F);
  }

}
