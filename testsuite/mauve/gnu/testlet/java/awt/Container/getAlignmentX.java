// Tags: JDK1.1
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

package gnu.testlet.java.awt.Container;

import java.awt.Container;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks if the getAlignmentX property works correctly.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class getAlignmentX implements Testlet
{

  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testWithLayout(harness);
  }

  /**
   * Checks if the method refers to the layout managers layoutAlignmentX
   * property.
   * 
   * @param harness the test harness to use
   */
  private void testWithLayout(TestHarness harness)
  {
    Container c = new Container();
    TestLayout l = new TestLayout();
    c.setLayout(l);
    l.alignmentX = 0.3F;
    harness.check(c.getAlignmentX(), 0.3F);
    l.alignmentX = 0.6F;
    harness.check(c.getAlignmentX(), 0.6F);
  }
}
