/* getElementIndex.java -- Checks the getElementIndex method in BranchElement
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

package gnu.testlet.javax.swing.text.AbstractDocument.BranchElement;

import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.PlainDocument;
import javax.swing.text.AbstractDocument.BranchElement;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks various aspects of the getElementIndex() method in BranchElement.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class getElementIndex implements Testlet
{

  /**
   * The entry point into this test.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testBeyondBoundary(harness);
  }

  /**
   * Checks how getElementIndex should behave when the requested index is
   * beyond the document boundary.
   *
   * @param h
   */
  private void testBeyondBoundary(TestHarness h)
  {
    PlainDocument doc = new PlainDocument();
    try
      {
        doc.insertString(0, "hello\n", null);
      }
    catch (BadLocationException ex)
      {
         h.fail(ex.getMessage());
      }
    Element root = doc.getDefaultRootElement();
    h.check(root.getElementIndex(6), 1);
  }
}
