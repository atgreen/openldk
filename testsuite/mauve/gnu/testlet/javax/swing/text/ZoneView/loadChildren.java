/* loadChildren.java -- Tests ZoneView.loadChildren().
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
import javax.swing.text.AbstractDocument.BranchElement;
import javax.swing.text.AbstractDocument.LeafElement;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class loadChildren implements Testlet
{

  public void test(TestHarness harness)
  {
    testSimple(harness);
    testOversize(harness);
  }

  private void testSimple(TestHarness h)
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
    TestZoneView zv = new TestZoneView(el, View.Y_AXIS);
    zv.loadChildren(zv.getViewFactory());
    h.check(zv.getViewCount(), 1);
    View child = zv.getView(0);
    h.check(child.getStartOffset(), 0);
    h.check(child.getEndOffset(), el.getEndOffset());
  }

  private void testOversize(TestHarness h)
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
    
    BranchElement el = (BranchElement) doc.getDefaultRootElement();
    // Modify the element structure a little and seen how the initial
    // elements are created then.
    LeafElement el1 = doc.new LeafElement(el, null, 0, 3);
    LeafElement el2 = doc.new LeafElement(el, null, 3, 7);
    LeafElement el3 = doc.new LeafElement(el, null, 7, 11);
    el.replace(0, 1, new Element[]{el1, el2, el3});


    TestZoneView zv = new TestZoneView(el, View.Y_AXIS);
    // Set the maximum zone size somewhere inside the second element.
    zv.setMaximumZoneSize(5);
    zv.loadChildren(zv.getViewFactory());
    h.check(zv.getViewCount(), 3);
    View child = zv.getView(0);
    h.check(child.getStartOffset(), 0);
    h.check(child.getEndOffset(), 3);
    child = zv.getView(1);
    h.check(child.getStartOffset(), 3);
    h.check(child.getEndOffset(), 7);
    child = zv.getView(2);
    h.check(child.getStartOffset(), 7);
    h.check(child.getEndOffset(), 11);
  }
}
