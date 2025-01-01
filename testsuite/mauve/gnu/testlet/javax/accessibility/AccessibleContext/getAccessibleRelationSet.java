/* getAccessibleRelationSet.java -- Tests the getAccesibleRelationSet method
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
// Uses: TestAccessibleContext

package gnu.testlet.javax.accessibility.AccessibleContext;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests the default implementation of
 * AccessibleContext.getAccessibleRelationSet().
 *
 * @author Roman Kennke (kennke@aicas.com)
 *
 */
public class getAccessibleRelationSet implements Testlet
{

  /**
   * The entry point in that class.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    TestAccessibleContext ctx = new TestAccessibleContext();
    harness.check(ctx.getAccessibleRelationSet() != null);
    harness.check(ctx.getAccessibleRelationSet().size(), 0);
  }

}
