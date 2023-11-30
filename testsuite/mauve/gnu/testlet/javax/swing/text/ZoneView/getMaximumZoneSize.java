/* getMaximumZoneSize.java -- Tests ZoneView.getMaximumZoneSize()
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

public class getMaximumZoneSize implements Testlet
{

  public void test(TestHarness h)
  {
    ZoneView zv = new TestZoneView();
    h.check(zv.getMaximumZoneSize(), 8192);
    zv.setMaximumZoneSize(0);
    h.check(zv.getMaximumZoneSize(), 0);
    zv.setMaximumZoneSize(1);
    h.check(zv.getMaximumZoneSize(), 1);
    zv.setMaximumZoneSize(Integer.MAX_VALUE);
    h.check(zv.getMaximumZoneSize(), Integer.MAX_VALUE);
    zv.setMaximumZoneSize(Integer.MIN_VALUE);
    h.check(zv.getMaximumZoneSize(), Integer.MIN_VALUE);
  }

}
