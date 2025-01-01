// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>
// Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.text.PlainDocument;

import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.PlainDocument;
import javax.swing.text.Position;
import javax.swing.text.SimpleAttributeSet;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests if insertString in PlainDocument works correctly.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class insertString implements Testlet
{

  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testNewline(harness);
    testFilterNewline(harness);
    testArguments(harness);
    testPositions(harness);

    testModifications(harness);
  }

  /**
   * Tests inserting a string with a newline. This should lead to a document
   * structure with one BranchElement as root, and two LeafElement as children.
   *
   * @param harness the test harness to use
   */
  private void testNewline(TestHarness harness)
  {
    harness.checkPoint("testNewline");

    PlainDocument doc = new PlainDocument();
    try
      {
        doc.insertString(0, "Hello\nWorld", new SimpleAttributeSet());
      }
    catch (BadLocationException ex)
      {
        harness.fail("BadLocationException");
      }
    Element root = doc.getRootElements()[0];
    harness.check(root instanceof AbstractDocument.BranchElement);
    harness.check(root.getElementCount(), 2);
    Element el1 = root.getElement(0);
    harness.check(el1 instanceof AbstractDocument.LeafElement);
    harness.check(el1.getStartOffset(), 0);
    harness.check(el1.getEndOffset(), 6);
    Element el2 = root.getElement(1);
    harness.check(el2 instanceof AbstractDocument.LeafElement);
    harness.check(el2.getStartOffset(), 6);
    harness.check(el2.getEndOffset(), 12);
  }

  /**
   * Tests inserting a string with a newline with the filterNewLine property
   * set to Boolean.TRUE. This should lead to a document
   * structure with one BranchElement as root, and one LeafElement as child,
   * spanning the whole content.
   *
   * @param harness the test harness to use
   */
  private void testFilterNewline(TestHarness harness)
  {
    harness.checkPoint("testFilterNewline");

    PlainDocument doc = new PlainDocument();
    doc.putProperty("filterNewlines", Boolean.TRUE);
    try
      {
        doc.insertString(0, "Hello\nWorld", new SimpleAttributeSet());
      }
    catch (BadLocationException ex)
      {
        harness.fail("BadLocationException");
      }
    Element root = doc.getRootElements()[0];
    harness.check(root instanceof AbstractDocument.BranchElement);
    harness.check(root.getElementCount(), 1);
    Element el1 = root.getElement(0);
    harness.check(el1 instanceof AbstractDocument.LeafElement);
    harness.check(el1.getStartOffset(), 0);
    harness.check(el1.getEndOffset(), 12);
  }
  
  public void testArguments(TestHarness harness)
  {
    harness.checkPoint("testArguments");
    PlainDocument d = new PlainDocument();
    
    // negative index
    boolean pass = false;
    try
    {
      d.insertString(-1, "XYZ", SimpleAttributeSet.EMPTY);
    }
    catch (Exception e)
    {
      pass = e instanceof BadLocationException;
    }
    harness.check(pass);

    // index > length
    pass = false;
    try
    {
      d.insertString(2, "XYZ", SimpleAttributeSet.EMPTY);
    }
    catch (BadLocationException e)
    {
      pass = true;
    }
    harness.check(pass);
  
    // null string is OK (ignored)
    pass = true;
    try
    {
      d.insertString(0, null, SimpleAttributeSet.EMPTY);
    }
    catch (Exception e)
    {
      pass = false;
    }
    harness.check(pass);
    
    // null attribute set is OK
    pass = true;
    try
    {
      d.insertString(0, "ABC", null);
    }
    catch (Exception e)
    {
      pass = false;
    }
    harness.check(pass);

  }
  
  public void testPositions(TestHarness harness) 
  {
    harness.checkPoint("testPositions");
    PlainDocument d = new PlainDocument();
    try
      {
        d.insertString(0, "ABC", null);
        Position p0 = d.createPosition(0);
        harness.check(p0.getOffset(), 0);
        Position p1 = d.createPosition(1);
        harness.check(p1.getOffset(), 1);
        Position p2 = d.createPosition(3);
        harness.check(p2.getOffset(), 3);
        Position p3 = d.createPosition(4);
        harness.check(p3.getOffset(), 4);
         
        d.insertString(1, "XYZ", null);
        harness.check(p0.getOffset(), 0);
        harness.check(p1.getOffset(), 4);
        harness.check(p2.getOffset(), 6);
        harness.check(p3.getOffset(), 7);
         
        d.remove(2, 3);
        harness.check(p0.getOffset(), 0);
        harness.check(p1.getOffset(), 2);
        harness.check(p2.getOffset(), 3);
        harness.check(p3.getOffset(), 4);      
      }
    catch (BadLocationException e)
      { 
      }
  }

  // Helper for testModifications.
  PlainDocument prepare(String initialContent)
  {
    PlainDocument pd = new PlainDocument();

    try
      {
        pd.insertString(0, initialContent, null);

        return pd;
      }
    catch (BadLocationException ble)
      {
        return pd;
      }
  }

  // Helper for testModifications.
  void checkElement(TestHarness harness,
		    PlainDocument doc,
 		    int elementIndex,
		    int startOffset,
		    int endOffset,
                    String text)
  {
    Element e = doc.getDefaultRootElement();
    Element child = e.getElement(elementIndex);

    harness.check(child.getStartOffset(), startOffset);
    harness.check(child.getEndOffset(), endOffset);

    String retrievedText = null;
    try
      {
        retrievedText = doc.getText(startOffset, endOffset-startOffset);
      }
    catch (BadLocationException ble)
      {
      }
    harness.check(retrievedText, text);
  }

  // Helper for testModifications.
  void insert(PlainDocument doc, int index, String text)
  {
    try
      {
        doc.insertString(index, text, null);
      }
    catch(BadLocationException ble)
      {
      }
  }

  void testModifications(TestHarness h)
  {
    // Test 1: Insert an "a" into before a "\n".
    h.checkPoint("modifications-insert char 1-pre");
    PlainDocument doc = new PlainDocument();;

    // Checks whether there is an Element at index 0 which has the
    // starting offset 0, end offset 1 and contains the text "\n".
    checkElement(h, doc, 0, 0, 1, "\n");

    h.checkPoint("modifications-insert char 1-post");
    insert(doc, 0, "a");
    checkElement(h, doc, 0, 0, 2, "a\n");

    // Test 2: Insert a newline after the first a in "abc\nbla\n".
    h.checkPoint("modifications-insert newline 1-pre");
    doc = prepare("abc\nbla\n");
    checkElement(h, doc, 0, 0, 4, "abc\n");
    checkElement(h, doc, 1, 4, 8, "bla\n");

    h.checkPoint("modifications-insert newline 1-post");
    insert(doc, 1, "\n");
    checkElement(h, doc, 0, 0, 2, "a\n");
    checkElement(h, doc, 1, 2, 5, "bc\n");
    checkElement(h, doc, 2, 5, 9, "bla\n");

    // Test 3: Insert a newline after the c in "abc\n".
    h.checkPoint("modifications-insert newline 2-pre");
    doc = prepare("abc\nbla\n");
    checkElement(h, doc, 0, 0, 4, "abc\n");

    h.checkPoint("modifications-insert newline 2-post");
    insert(doc, 3, "\n");
    checkElement(h, doc, 0, 0, 4, "abc\n");
    checkElement(h, doc, 1, 4, 5, "\n");

    // Test 4: Type a char after "abc\n".
    h.checkPoint("modifications-insert char 2-pre");
    doc = prepare("abc\n");
    checkElement(h, doc, 0, 0, 4, "abc\n");

    h.checkPoint("modifications-insert char 2-post");
    insert(doc, 4, "d");
    checkElement(h, doc, 0, 0, 4, "abc\n");
    checkElement(h, doc, 1, 4, 6, "d\n");

    // Test 5: Insert "foo\nbaz\nbar" after "ab" in "abc\ndef\n".
    h.checkPoint("modifications-insert multi-line string 1-pre");
    doc = prepare("abc\ndef\n");
    checkElement(h, doc, 0, 0, 4, "abc\n");
    checkElement(h, doc, 1, 4, 8, "def\n");

    h.checkPoint("modifications-insert multi-line string 1-post");
    insert(doc, 2, "foo\nbaz\nbar");
    checkElement(h, doc, 0,  0,  6, "abfoo\n");
    checkElement(h, doc, 1,  6, 10, "baz\n");
    checkElement(h, doc, 2, 10, 15, "barc\n");
    checkElement(h, doc, 3, 15, 19, "def\n");

    // Test 6: Insert "foo" after first newline in "abc\ndef\n"
    h.checkPoint("modifications-insert single-line string-pre");
    doc = prepare("abc\ndef\n");
    checkElement(h, doc, 0, 0, 4, "abc\n");
    checkElement(h, doc, 1, 4, 8, "def\n");

    h.checkPoint("modifications-insert single-line string-post");
    insert(doc, 4, "foo");
    checkElement(h, doc, 0,  0,  4, "abc\n");
    checkElement(h, doc, 1,  4, 11, "foodef\n");

    // Test 7: Insert "foo\nbaz\nbar" after first newline in "abc\ndef\n".
    h.checkPoint("modifications-insert multi-line string 2-pre");
    doc = prepare("abc\ndef\n");
    checkElement(h, doc, 0, 0, 4, "abc\n");
    checkElement(h, doc, 1, 4, 8, "def\n");

    h.checkPoint("modifications-insert multi-line string 2-post");
    insert(doc, 4, "foo\nbaz\nbar");
    checkElement(h, doc, 0,  0,  4, "abc\n");
    checkElement(h, doc, 1,  4,  8, "foo\n");
    checkElement(h, doc, 2,  8, 12, "baz\n");
    checkElement(h, doc, 3, 12, 19, "bardef\n");

    // Test 8: Type char after a in "ac\n".
    h.checkPoint("modifications-insert char 3-pre");
    doc = prepare("ac\n");
    checkElement(h, doc, 0, 0, 3, "ac\n");

    h.checkPoint("modifications-insert char 3-post");
    insert(doc, 1, "b");
    checkElement(h, doc, 0, 0, 4, "abc\n");

    // Test 9: Multiple text insertions
    h.checkPoint("modifications-multi-insert-1");
    doc = prepare("abc\ndef\n");
    checkElement(h, doc, 0, 0, 4, "abc\n");
    checkElement(h, doc, 1, 4, 8, "def\n");

    h.checkPoint("modifications-multi-insert-2");
    insert(doc, 3, "---");
    checkElement(h, doc, 0, 0, 7, "abc---\n");
    checkElement(h, doc, 1, 7, 11, "def\n");

    h.checkPoint("modifications-multi-insert-3");
    insert(doc, 7, "---");
    checkElement(h, doc, 0, 0, 7, "abc---\n");
    checkElement(h, doc, 1, 7, 14, "---def\n");
  }

}
