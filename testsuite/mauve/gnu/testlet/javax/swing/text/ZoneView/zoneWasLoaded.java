/* zoneWasLoaded.java -- Checks ZoneView.zoneWasLoaded()
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

public class zoneWasLoaded implements Testlet
{

  public void test(TestHarness h)
  {
    TestZoneView zv = new TestZoneView();
    zv.setMaxZonesLoaded(3);
    TestView v1 = new TestView();
    zv.lastUnloadedZones.clear();
    // 3 Zones are allowed to be loaded. So load them and check that none
    // is unloaded.
    zv.zoneWasLoaded(v1);
    h.check(zv.lastUnloadedZones.size(), 0);
    TestView v2 = new TestView();
    zv.zoneWasLoaded(v2);
    h.check(zv.lastUnloadedZones.size(), 0);
    TestView v3 = new TestView();
    zv.zoneWasLoaded(v3);
    h.check(zv.lastUnloadedZones.size(), 0);
    // After loading this zone, the first one must get unloaded.
    TestView v4 = new TestView();
    zv.zoneWasLoaded(v4);
    h.check(zv.lastUnloadedZones.size(), 1);
    h.check(zv.lastUnloadedZones.remove(0), v1);
    // Then the second.
    TestView v5 = new TestView();
    zv.zoneWasLoaded(v5);
    h.check(zv.lastUnloadedZones.size(), 1);
    h.check(zv.lastUnloadedZones.remove(0), v2);
    // And then the third.
    TestView v6 = new TestView();
    zv.zoneWasLoaded(v6);
    h.check(zv.lastUnloadedZones.size(), 1);
    h.check(zv.lastUnloadedZones.remove(0), v3);

    // Also check for null argument.
    try
      {
        // Push out v4, v5 and v6 and see if we get to unload(null) after this.
        zv.zoneWasLoaded(null);
        zv.zoneWasLoaded(null);
        zv.zoneWasLoaded(null);
        h.check(true);
      }
    catch (NullPointerException ex)
      {
        h.fail("No NullPointerException should be thrown");
      }
    try
      {
        // The above statements pushed 3 nulls in there, which get pulled now
        // and trigger an NPE.
        zv.zoneWasLoaded(v1);
        h.fail("NullPointerException should be thrown");
      }
    catch (NullPointerException ex)
      {
        h.check(true);
      }
  }

}
