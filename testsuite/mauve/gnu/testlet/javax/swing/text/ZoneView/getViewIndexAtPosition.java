/* getViewIndexAtPosition.java -- Checks ZoneViewgetViewIndexAtPosition()
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

import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.PlainDocument;
import javax.swing.text.View;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class getViewIndexAtPosition implements Testlet
{

  public void test(TestHarness h)
  {
    PlainDocument doc = new PlainDocument();
    try
      {
        doc.insertString(0, "0123456789", null);
      }
    catch (BadLocationException ex)
      {
        RuntimeException rte = new RuntimeException();
        rte.initCause(ex);
        throw rte;
      }
    Element el = doc.getDefaultRootElement();
    TestZoneView zv = new TestZoneView(el, View.X_AXIS);
    zv.append(zv.createZone(0, 3));
    zv.append(zv.createZone(3, 7));
    zv.append(zv.createZone(7, 10));

    h.check(zv.getViewIndexAtPosition(-100), -1);
    h.check(zv.getViewIndexAtPosition(-1), -1);
    h.check(zv.getViewIndexAtPosition(0), 0);
    h.check(zv.getViewIndexAtPosition(2), 0);
    h.check(zv.getViewIndexAtPosition(3), 1);
    h.check(zv.getViewIndexAtPosition(6), 1);
    h.check(zv.getViewIndexAtPosition(7), 2);
    h.check(zv.getViewIndexAtPosition(9), 2);
    // This is a noteworthy detail: The offset 10 isn't actually covered
    // by any child view, but the endOffset of the view (11) yields the index
    // of the last view in the RI. I think this is a bug in Sun's impl as the
    // method should return -1 if the offset isn't covered by any child view.
    h.check(zv.getViewIndexAtPosition(10), -1);
    // The RI does the following:
    //h.check(zv.getViewIndexAtPosition(11), 2);
    h.check(zv.getViewIndexAtPosition(11), -1);
    h.check(zv.getViewIndexAtPosition(100), -1);
  }

}
