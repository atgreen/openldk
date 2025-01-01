/* isZoneLoaded.java -- Checks ZoneView.isZoneLoaded()
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

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class isZoneLoaded implements Testlet
{

  public void test(TestHarness h)
  {
    // Note that we don't add the view to the ZoneView. The ZoneView
    // only checks if the view has children.
    TestZoneView zv = new TestZoneView();
    // Create a view without children.
    TestView v = new TestView();
    h.check(zv.isZoneLoaded(v), false);
    TestView child = new TestView();
    v.append(child);
    h.check(zv.isZoneLoaded(v), true);
  }

}
