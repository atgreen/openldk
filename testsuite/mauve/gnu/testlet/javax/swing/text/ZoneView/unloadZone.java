/* unloadZone.java -- Tests ZoneView.unloadZone()
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

// Tags: JDK1.3
// Uses: TestView TestZoneView

package gnu.testlet.javax.swing.text.ZoneView;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class unloadZone implements Testlet
{

  public void test(TestHarness h)
  {
    // This method should simply call removeAll() on the zone view.
    // So we test this.
    TestZoneView zv = new TestZoneView();
    TestView v = new TestView();
    v.removeAllCalled = false;
    zv.unloadZone(v);
    h.check(v.removeAllCalled, true);
    // Also check for null argument.
    try
      {
        zv.unloadZone(null);
        h.fail("NullPointerException should be thrown");
      }
    catch (NullPointerException ex)
      {
        h.check(true);
      }
  }

}
