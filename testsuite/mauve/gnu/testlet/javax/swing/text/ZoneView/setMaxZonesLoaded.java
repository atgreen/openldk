/* setMaxZonesLoaded.java -- Tests ZoneView.setMaxZonesLoaded()
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
// Uses: TestZoneView

package gnu.testlet.javax.swing.text.ZoneView;

import javax.swing.text.ZoneView;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class setMaxZonesLoaded implements Testlet
{

  public void test(TestHarness h)
  {
    testSimple(h);
    testUnloading(h);
  }

  private void testSimple(TestHarness h)
  {
    ZoneView zv = new TestZoneView();
    // Test legal values.
    zv.setMaxZonesLoaded(1);
    h.check(zv.getMaxZonesLoaded(), 1);
    zv.setMaxZonesLoaded(Integer.MAX_VALUE);
    h.check(zv.getMaxZonesLoaded(), Integer.MAX_VALUE);
    // Test illegal values.
    try
      {
        zv.setMaxZonesLoaded(0);
        h.fail("Should have thrown IllegalArgumentException");
      }
    catch (IllegalArgumentException ex)
      {
        h.check(true);
      }
    try
      {
        zv.setMaxZonesLoaded(Integer.MIN_VALUE);
        h.fail("Should have thrown IllegalArgumentException");
      }
    catch (IllegalArgumentException ex)
      {
        h.check(true);
      }
  }

  private void testUnloading(TestHarness h)
  {
    TestZoneView zv = new TestZoneView();
    // Add 4 zones, which are the number of allowed zones.
    zv.setMaxZonesLoaded(4);
    TestView v1 = new TestView();
    zv.zoneWasLoaded(v1);
    TestView v2 = new TestView();
    zv.zoneWasLoaded(v2);
    TestView v3 = new TestView();
    zv.zoneWasLoaded(v3);
    TestView v4 = new TestView();
    zv.zoneWasLoaded(v4);
    // Zero zones unloaded so far.
    h.check(zv.lastUnloadedZones.size(), 0);
    zv.setMaxZonesLoaded(2);
    // The first two zones are unloaded.
    h.check(zv.lastUnloadedZones.size(), 2);
    h.check(zv.lastUnloadedZones.remove(0), v1);
    h.check(zv.lastUnloadedZones.remove(0), v2);
    // No zones get unloaded when growing the allowed range.
    zv.setMaxZonesLoaded(4);
    h.check(zv.lastUnloadedZones.size(), 0);
    // The v3 gets unloaded when shrinking to 1.
    zv.setMaxZonesLoaded(1);
    h.check(zv.lastUnloadedZones.size(), 1);
    h.check(zv.lastUnloadedZones.remove(0), v3);
  }
}
