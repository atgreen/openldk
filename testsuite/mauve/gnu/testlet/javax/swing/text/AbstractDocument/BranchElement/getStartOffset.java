/* getStartOffset.java -- Checks the getStartOffset method in BranchElement
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
import javax.swing.text.Element;
import javax.swing.text.PlainDocument;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class getStartOffset implements Testlet
{

  /**
   * The entry point in this test.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testEmptyBranchElement(harness);
    testOneLeafElement(harness);
    testCachedValue(harness);
  }

  /**
   * Creates an empty BranchElement and checks if the getStartOffset() method
   * call triggers an NPE.
   *
   * @param h the test harness to use
   */
  private void testEmptyBranchElement(TestHarness h)
  {
    PlainDocument doc = new PlainDocument();
    AbstractDocument.BranchElement b = 
      doc.new BranchElement(null, null);
    try
      {
        b.getStartOffset();
        h.fail("emptyBranchElement");
      }
    catch (NullPointerException ex)
      {
        h.check(true);
      }
  }

  /**
   * Creates a BranchElement with one LeafElement child and checks if the
   * getStartOffset() method call returns the correct start offset.
   *
   * @param h the test harness to use
   */
  private void testOneLeafElement(TestHarness h)
  {
    PlainDocument doc = new PlainDocument();
    AbstractDocument.BranchElement b = 
      doc.new BranchElement(null, null);
    AbstractDocument.LeafElement l = doc.new LeafElement(b, null, 10, 20);
    b.replace(0, 0, new Element[] { l });
    h.check(b.getStartOffset(), 10);
  }

  /**
   * This first creates a BranchElement with one child and calls
   * getStartOffset(). Then it removes the child and calls startOffset() again
   * and checks if the BranchElement still returns a valid value. The
   * BranchElement should remember the value and don't throw an NPE here.
   *
   * @param h the test harness to use
   */
  private void testCachedValue(TestHarness h)
  {
    PlainDocument doc = new PlainDocument();
    AbstractDocument.BranchElement b = 
      doc.new BranchElement(null, null);
    AbstractDocument.LeafElement l = doc.new LeafElement(b, null, 10, 20);
    b.replace(0, 0, new Element[] { l });
    h.check(b.getStartOffset(), 10);

    b.replace(0, 1, new Element[0]);
    h.check(b.getElementCount(), 0);
    h.check(b.getStartOffset(), 10);
  }
}
