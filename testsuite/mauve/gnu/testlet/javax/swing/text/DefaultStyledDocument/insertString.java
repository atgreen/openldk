// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke (kennke@aicas.com)

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.text.DefaultStyledDocument;

import java.awt.Color;

import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests if content is inserted correctly into the document.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class insertString implements Testlet
{
  /**
   * Entry point. This calls the single tests.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testInsertEqualAttributes(harness);
    testInsertModifiedAttributes(harness);
    testInsertNewline(harness);
  }

  /**
   * Inserting content with equal attributes should not create
   * new elements. In this test we start a single child elemen (0, 16)
   * and insert 5 characters at position 5. This should result in
   * a single child element (0, 21).
   */
  void testInsertEqualAttributes(TestHarness harness)
  {
    harness.checkPoint("insertEqualAttributes");
    DefaultStyledDocument doc = new DefaultStyledDocument();
    prepareDocument(doc);
    SimpleAttributeSet atts = new SimpleAttributeSet();

    // Insert 5 characters at pos 5.
    try
      {
        doc.insertString(5, "12345", atts);
      }
    catch (BadLocationException ex)
      {
        harness.debug(ex);
      }

    // Now we should have the following child elements below the single
    // root and single paragraph: (0, 5) (5, 10) (10, 21)
    Element root = doc.getDefaultRootElement();
    harness.check(root.getStartOffset(), 0);
    harness.check(root.getEndOffset(), 21);
    harness.check(root.getElementCount(), 1);

    Element par = root.getElement(0);
    harness.check(par.getStartOffset(), 0);
    harness.check(par.getEndOffset(), 21);
    harness.check(par.getElementCount(), 1);

    Element child1 = par.getElement(0);
    harness.check(child1.getStartOffset(), 0);
    harness.check(child1.getEndOffset(), 21);
  }

  /**
   * If a chunk of content is inserted with modified attributes, then
   * the resulting element structure should reflect that. In this test
   * we start with a single child element (0, 16) and insert 5 characters
   * (with different attributes) at position 5. This should lead
   * to a structure of (0, 5)(5, 10)(10, 21).
   *
   * @param harness the test harness to use
   */
  void testInsertModifiedAttributes(TestHarness harness)
  {
    harness.checkPoint("insertModifiedAttributes");
    DefaultStyledDocument doc = new DefaultStyledDocument();
    prepareDocument(doc);
    SimpleAttributeSet atts = new SimpleAttributeSet();

    // Insert 5 (different from the rest) characters at pos 5.
    StyleConstants.setForeground(atts, Color.RED);
    try
      {
        doc.insertString(5, "12345", atts);
      }
    catch (BadLocationException ex)
      {
        harness.debug(ex);
      }

    // Now we should have the following child elements below the single
    // root and single paragraph: (0, 5) (5, 10) (10, 21)
    Element root = doc.getDefaultRootElement();
    harness.check(root.getStartOffset(), 0);
    harness.check(root.getEndOffset(), 21);
    harness.check(root.getElementCount(), 1);

    Element par = root.getElement(0);
    harness.check(par.getStartOffset(), 0);
    harness.check(par.getEndOffset(), 21);
    harness.check(par.getElementCount(), 3);

    Element child1 = par.getElement(0);
    harness.check(child1.getStartOffset(), 0);
    harness.check(child1.getEndOffset(), 5);

    Element child2 = par.getElement(1);
    harness.check(child2.getStartOffset(), 5);
    harness.check(child2.getEndOffset(), 10);

    Element child3 = par.getElement(2);
    harness.check(child3.getStartOffset(), 10);
    harness.check(child3.getEndOffset(), 21);
  }

  /**
   * Here we try to insert content with a line break in it. This should
   * break up the paragraph.
   *
   * We start with one single paragraph which has a single child element
   * (0, 16). Then we insert "abcde\nfghij" at position 5. This should
   * lead to two paragraphs: one that spans from 0 .. 11 and one that
   * spans from 11 .. 27. Each of the paragraphs should have one child
   * element that has the same spans.
   */
  void testInsertNewline(TestHarness harness)
  {
    harness.checkPoint("insertNewline");
    DefaultStyledDocument doc = new DefaultStyledDocument();
    prepareDocument(doc);
    SimpleAttributeSet atts = new SimpleAttributeSet();

    // Insert 5 (different from the rest) characters at pos 5.
    try
      {
        doc.insertString(5, "abcde\nfghij", atts);
      }
    catch (BadLocationException ex)
      {
        harness.debug(ex);
      }

    // Now we should have two paragraphs (0, 11)(11, 27)
    Element root = doc.getDefaultRootElement();
    harness.check(root.getStartOffset(), 0);
    harness.check(root.getEndOffset(), 27);
    harness.check(root.getElementCount(), 2);

    Element par1 = root.getElement(0);
    harness.check(par1.getStartOffset(), 0);
    harness.check(par1.getEndOffset(), 11);
    harness.check(par1.getElementCount(), 1);

    Element par2 = root.getElement(1);
    harness.check(par2.getStartOffset(), 11);
    harness.check(par2.getEndOffset(), 27);
    harness.check(par2.getElementCount(), 1);
  }

  /**
   * Inserts some content into the document. This content has exactly
   * 15 characters, which make 16 at all. The last one doesn't really
   * count though, but we have to take care of details.
   *
   * @param doc the document to prepare
   */
  void prepareDocument(DefaultStyledDocument doc)
  {
    try
      {
        doc.insertString(0, "abcdefghijklmno", new SimpleAttributeSet());
      }
    catch (BadLocationException ex)
      {
        ex.printStackTrace();
      }
  }
}
