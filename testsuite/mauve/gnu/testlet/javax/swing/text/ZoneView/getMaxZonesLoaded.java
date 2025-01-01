/* getMaxZonesLoaded.java -- Checks ZoneView.getMaxZonesLoaded()
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

package gnu.testlet.javax.swing.text.ZoneView;

import gnu.testlet.TestHarness;

import javax.swing.text.ZoneView;

public class getMaxZonesLoaded
{
  public void test(TestHarness h)
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
    h.check(zv.getMaxZonesLoaded(), Integer.MAX_VALUE);
    try
      {
        zv.setMaxZonesLoaded(Integer.MIN_VALUE);
        h.fail("Should have thrown IllegalArgumentException");
      }
    catch (IllegalArgumentException ex)
      {
        h.check(true);
      }
    h.check(zv.getMaxZonesLoaded(), Integer.MAX_VALUE);
  }
}
