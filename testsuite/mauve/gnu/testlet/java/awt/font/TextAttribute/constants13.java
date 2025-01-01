// Tags: JDK1.3

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.java.awt.font.TextAttribute;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.font.TextAttribute;

/**
 * Some tests for the constants in the {@link TextAttribute} class.
 */
public class constants13 implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.check(TextAttribute.UNDERLINE_LOW_DASHED, new Integer(5));
    harness.check(TextAttribute.UNDERLINE_LOW_DOTTED, new Integer(3));
    harness.check(TextAttribute.UNDERLINE_LOW_GRAY, new Integer(4));
    harness.check(TextAttribute.UNDERLINE_LOW_ONE_PIXEL, new Integer(1));
    harness.check(TextAttribute.UNDERLINE_LOW_TWO_PIXEL, new Integer(2));
  } 

}