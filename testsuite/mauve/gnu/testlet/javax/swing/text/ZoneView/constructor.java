/* constructor.java -- Tests the constructor of ZoneView
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

import javax.swing.text.Element;
import javax.swing.text.PlainDocument;
import javax.swing.text.View;
import javax.swing.text.ZoneView;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class constructor implements Testlet
{

  public void test(TestHarness harness)
  {
    testSimple(harness);
    testDefaultValues(harness);
  }

  /**
   * Some very simple tests.
   *
   * @param h the test harness
   */
  private void testSimple(TestHarness h)
  {
    PlainDocument doc = new PlainDocument();
    Element el = doc.getDefaultRootElement();
    ZoneView zv = new ZoneView(el, View.X_AXIS);
    h.check(zv.getAxis(), View.X_AXIS);
    h.check(zv.getElement(), el);
    zv = new ZoneView(el, View.Y_AXIS);
    h.check(zv.getAxis(), View.Y_AXIS);
    h.check(zv.getElement(), el);
  }

  private void testDefaultValues(TestHarness h)
  {
    PlainDocument doc = new PlainDocument();
    Element el = doc.getDefaultRootElement();
    ZoneView zv = new ZoneView(el, View.X_AXIS);
    h.check(zv.getMaximumZoneSize(), 8192);
    h.check(zv.getMaxZonesLoaded(), 3);
  }
}
