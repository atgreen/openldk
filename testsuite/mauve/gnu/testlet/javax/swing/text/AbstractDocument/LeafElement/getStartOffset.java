/* getStartOffset.java -- Checks getStartOffset in LeafElement
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

package gnu.testlet.javax.swing.text.AbstractDocument.LeafElement;

import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.PlainDocument;
import javax.swing.text.AbstractDocument.BranchElement;
import javax.swing.text.AbstractDocument.LeafElement;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks the getStartOffset method in LeafElement.
 *
 * @author Roman Kennke (kennke@aicas.com)
 *
 */
public class getStartOffset implements Testlet
{

  /**
   * The entry point in this test.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testIllegalDocumentLocation(harness);
    testContentChange(harness);
  }

  /**
   * Creates a LeafElement outside the document range and checks the
   * getStartOffset() behaviour.
   *
   * @param h the test harness to use
   */
  private void testIllegalDocumentLocation(TestHarness h)
  {
    h.checkPoint("illegalDocumentLocation");
    PlainDocument doc = new PlainDocument();
    AbstractDocument.LeafElement l = doc.new LeafElement(null, null, 10, 20);
    h.check(doc.getLength(), 0);
    h.check(l.getStartOffset(), 10);
  }

  /**
   * Creates a LeafElement and then changes something in the document. Tests
   * if the getStartOffset() automatically adapts to the insertion or not.
   *
   * @param h the test harness to use
   */
  private void testContentChange(TestHarness h)
  {
    h.checkPoint("contentChange");
    try
      {
        PlainDocument doc = new PlainDocument();
        doc.insertString(0, "hallo", null);
        AbstractDocument.LeafElement l = doc.new LeafElement(null, null, 20, 5);
        h.check(l.getStartOffset(), 20);
        doc.insertString(0, "hiyo", null);
        h.check(l.getStartOffset(), 24);
      }
    catch (BadLocationException ex)
      {
        h.fail("BadLocationException");
      }
  }
}
