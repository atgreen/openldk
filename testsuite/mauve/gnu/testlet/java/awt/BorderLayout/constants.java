// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

// This file is part of Mauve.

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

package gnu.testlet.java.awt.BorderLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.BorderLayout;

/**
 * Some checks for the constants defined by the {@link BorderLayout} class.  
 */
public class constants implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    harness.check(BorderLayout.AFTER_LAST_LINE.equals(BorderLayout.PAGE_END));
    harness.check(BorderLayout.AFTER_LINE_ENDS.equals(BorderLayout.LINE_END));
    harness.check(BorderLayout.BEFORE_FIRST_LINE.equals(BorderLayout.PAGE_START));
    harness.check(BorderLayout.BEFORE_LINE_BEGINS.equals(BorderLayout.LINE_START));
    harness.check(BorderLayout.CENTER.equals("Center"));
    harness.check(BorderLayout.EAST.equals("East"));
    harness.check(BorderLayout.LINE_END.equals("After"));
    harness.check(BorderLayout.LINE_START.equals("Before"));
    harness.check(BorderLayout.NORTH.equals("North"));
    harness.check(BorderLayout.PAGE_END.equals("Last"));
    harness.check(BorderLayout.PAGE_START.equals("First"));
    harness.check(BorderLayout.SOUTH.equals("South"));
    harness.check(BorderLayout.WEST.equals("West"));
  }

}
