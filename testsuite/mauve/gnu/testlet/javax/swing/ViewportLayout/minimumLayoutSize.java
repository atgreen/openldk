/* minimumLayoutSize.java -- Tests the minimumLayoutSize of the ViewportLayout
   Copyright (C) 2006 Roman Kennke (kennke@aicas.com)
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.ViewportLayout;


import java.awt.Dimension;

import javax.swing.ViewportLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests the minimumLayoutSize() method of the ViewportLayout.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class minimumLayoutSize implements Testlet
{

  /**
   * The entry point into this test.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    ViewportLayout l = new ViewportLayout();
    Dimension minSize = l.minimumLayoutSize(null);
    harness.check(minSize, new Dimension(4, 4));
  }

}
