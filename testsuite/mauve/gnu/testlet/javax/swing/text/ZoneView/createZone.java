/* createZone.java -- Tests ZoneView.createZone()
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

import javax.swing.text.AsyncBoxView;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.PlainDocument;
import javax.swing.text.View;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class createZone implements Testlet
{

  public void test(TestHarness h)
  {
    PlainDocument doc = new PlainDocument();
    try
      {
        doc.insertString(0, "123456789", null);
      }
    catch (BadLocationException ex)
      {
        RuntimeException rte = new RuntimeException();
        rte.initCause(ex);
        throw rte;
      }
    Element el = doc.getDefaultRootElement();
    TestZoneView zv = new TestZoneView(el, View.X_AXIS);

    // Try valid zone.
    View zone = zv.createZone(0, 9);
    h.check(zone instanceof AsyncBoxView);
    h.check(zone.getStartOffset(), 0);
    h.check(zone.getEndOffset(), 9);
    // Check if the zone follows document insertions (via Positions).
    try
      {
        doc.insertString(5, "abcde", null);
      }
    catch (BadLocationException ex)
      {
        RuntimeException rte = new RuntimeException();
        rte.initCause(ex);
        throw rte;
      }
    h.check(zone.getStartOffset(), 0);
    h.check(zone.getEndOffset(), 14);

    // Try invalid zone. No error is thrown and the zone tracks some more
    // or less random positions. This is probably due to a bug in the Content
    // implementation of Sun, but anyway. We don't check the exact positions
    // here as it doesn't really matter anyway. This is unspecified!
    zone = zv.createZone(-10, 23);
  }

}
