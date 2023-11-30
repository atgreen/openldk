/* retainAll.java -- Checks functionality in retainAll()
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

package gnu.testlet.java.util.Vector;

import java.util.Vector;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class retainAll implements Testlet
{

  public void test(TestHarness harness)
  {
    testNull(harness);
  }

  /**
   * Checks if and when null arguments to removeAll() are allowed.
   *
   * @param h the test harness
   */
  private void testNull(TestHarness h)
  {
    // Check empty vector.
    Vector v = new Vector();
    v.removeAll(null);
    h.check(true); // If we got here, there was no NPE.

    // Check non-empty vector.
    v.add(new Object());
    try
      {
        v.retainAll(null);
        h.fail("NPE should be thrown");
      }
    catch (NullPointerException ex)
      {
        h.check(true);
      }
  }
}
