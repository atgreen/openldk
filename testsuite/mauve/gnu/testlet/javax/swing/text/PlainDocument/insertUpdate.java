/* insertUpdate.java -- Tests PlainDocument.insertUpdate()
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.text.PlainDocument;

import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.PlainDocument;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests PlainDocument.insertUpdate().
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class insertUpdate implements Testlet
{

  private class TestListener
    implements DocumentListener
  {
    public void changedUpdate(DocumentEvent e)
    {
      ev = e;
    }
    public void insertUpdate(DocumentEvent e)
    {
      ev = e;
    }
    public void removeUpdate(DocumentEvent e)
    {
      ev = e;
    }

  }

  DocumentEvent ev;

  public void test(TestHarness harness)
  {
    test01(harness);
    test02(harness);
    test03(harness);
  }

  private void test01(TestHarness h)
  {
    h.checkPoint("test01");
    PlainDocument doc = new PlainDocument();
    doc.addDocumentListener(new TestListener());

    try
      {
        doc.insertString(0, "abcde\nabcdef\nabcde\n", null);
        doc.insertString(15, "abcdefghijklmn\n", null);
      }
    catch (BadLocationException ex)
      {
        h.fail("Unexpected BadLocationException");
      }

    DocumentEvent.ElementChange change = ev.getChange(doc.getDefaultRootElement());

    h.check(change.getIndex(), 2);

    Element[] added = change.getChildrenAdded();
    h.check(added.length, 2);
    h.check(added[0].getStartOffset(), 13);
    h.check(added[0].getEndOffset(), 30);
    h.check(added[1].getStartOffset(), 30);
    h.check(added[1].getEndOffset(), 34);

    Element[] removed = change.getChildrenRemoved();
    h.check(removed.length, 1);
    h.check(removed[0].getStartOffset(), 13);
    h.check(removed[0].getEndOffset(), 34);
  }

  private void test02(TestHarness h)
  {
    h.checkPoint("test02");
    PlainDocument doc = new PlainDocument();
    doc.addDocumentListener(new TestListener());

    try
      {
        doc.insertString(0, "abcde\nabcdef\nabcde\n", null);
        doc.insertString(13, "\nabc", null);
      }
    catch (BadLocationException ex)
      {
        h.fail("Unexpected BadLocationException");
      }

    DocumentEvent.ElementChange change = ev.getChange(doc.getDefaultRootElement());

    h.check(change.getIndex(), 1);

    Element[] added = change.getChildrenAdded();
    h.check(added.length, 3);
    h.check(added[0].getStartOffset(), 6);
    h.check(added[0].getEndOffset(), 13);
    h.check(added[1].getStartOffset(), 13);
    h.check(added[1].getEndOffset(), 14);
    h.check(added[2].getStartOffset(), 14);
    h.check(added[2].getEndOffset(), 23);

    Element[] removed = change.getChildrenRemoved();
    h.check(removed.length, 2);
    h.check(removed[0].getStartOffset(), 6);
    h.check(removed[0].getEndOffset(), 17);
    h.check(removed[1].getStartOffset(), 17);
    h.check(removed[1].getEndOffset(), 23);
  }

  private void test03(TestHarness h)
  {
    h.checkPoint("test03");
    PlainDocument doc = new PlainDocument();
    doc.addDocumentListener(new TestListener());

    try
      {
        doc.insertString(0, "abcd", null);
        doc.insertString(0, "abcde\nabcdef\nabcde\n", null);
      }
    catch (BadLocationException ex)
      {
        h.fail("Unexpected BadLocationException");
      }

    DocumentEvent.ElementChange change = ev.getChange(doc.getDefaultRootElement());

    h.check(change.getIndex(), 0);

    Element[] added = change.getChildrenAdded();
    h.check(added.length, 4);
    h.check(added[0].getStartOffset(), 0);
    h.check(added[0].getEndOffset(), 6);
    h.check(added[1].getStartOffset(), 6);
    h.check(added[1].getEndOffset(), 13);
    h.check(added[2].getStartOffset(), 13);
    h.check(added[2].getEndOffset(), 19);
    h.check(added[3].getStartOffset(), 19);
    h.check(added[3].getEndOffset(), 24);

    Element[] removed = change.getChildrenRemoved();
    h.check(removed.length, 1);
    h.check(removed[0].getStartOffset(), 0);
    h.check(removed[0].getEndOffset(), 24);
  }
  
}
