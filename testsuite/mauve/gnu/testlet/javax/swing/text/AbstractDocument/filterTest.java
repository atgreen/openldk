// Tags: JDK1.2

// Copyright (C) 2006 Robert Schuster (robertschuster@fsfe.org)

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

package gnu.testlet.javax.swing.text.AbstractDocument;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.swing.text.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.undo.*;

/**
 * Tests if AbstractDocument methods properly call the DocumentFilter and
 * GapContent (or not when the arguments have certain values).
 */
public class filterTest
  implements Testlet
{

  int DEFAULT_INT = -1234;
  String DEFAULT_STRING = "A specific value for this test";
  AttributeSet DEFAULT_ATTRIBUTESET = new SimpleAttributeSet();

  int testOffset;
  int testLength;
  String testString;
  AttributeSet testAttr;
  Exception testException;
  int testWhere, testInsertWhere;
  int testNitems;

  public void test(TestHarness harness)
  {
    // The behavior we want to test is in AbstractDocument
    // of which we just use PlainDocument as its simplest
    // concrete implementation.
    AbstractDocument doc = new PlainDocument();

    // Put some example text into the document.
    try
      {
        doc.insertString(0, "GNU Classpath\nGNU Classpath\nGNU Classpath\n", null);
      }
    catch (BadLocationException ble)
      {
        harness.verbose("Setting up the test failed. Something is seriously "
                        + "wrong with your AbstractDocument implementation.");
        harness.fail("test setup");
        return;
      }

    // Install the documen filter which copies the argument values.
    doc.setDocumentFilter(new TestDocFilter());

    harness.checkPoint("remove");

    // Removal check 1: Although this would have no effect on the document
    // the DocumentListener is called. However the GapContent should not be
    // called.
    reset();
    try
      {
        doc.remove(0, 0);
      }
    catch (BadLocationException ble)
      {
        testException = ble;
      }

    harness.check(testException, null);
    harness.check(testOffset, 0);
    harness.check(testLength, 0);
    harness.check(testWhere, DEFAULT_INT);
    harness.check(testNitems, DEFAULT_INT);

    // Removal check 2: A negative length value is carried to the
    // DocumentFilter but not to the GapContent.
    reset();
    try
      {
        doc.remove(0, -100);
      }
    catch (BadLocationException ble)
      {
        testException = ble;
	harness.debug(ble);
      }

    harness.check(testException, null);
    harness.check(testOffset, 0);
    harness.check(testLength, -100);
    harness.check(testWhere, DEFAULT_INT);
    harness.check(testNitems, DEFAULT_INT);

    // Removal check 3: A negative offset and length value is carried to
    // the DocumentFilter but not to the GapContent.
    reset();
    try
      {
        doc.remove(-222, -111);
      }
    catch (BadLocationException ble)
      {
        testException = ble;
	harness.debug(ble);
      }

    harness.check(testException, null);
    harness.check(testOffset, -222);
    harness.check(testLength, -111);
    harness.check(testWhere, DEFAULT_INT);
    harness.check(testNitems, DEFAULT_INT);

    // Removal check 4: A negative offset value is carried to the
    // DocumentFilter but not to the GapContent.
    reset();
    try
      {
        doc.remove(-100, 0);
      }
    catch (BadLocationException ble)
      {
        testException = ble;
	harness.debug(ble);
      }

    harness.check(testException, null);
    harness.check(testOffset, -100);
    harness.check(testLength, 0);
    harness.check(testWhere, DEFAULT_INT);
    harness.check(testNitems, DEFAULT_INT);

    harness.checkPoint("insertString");

    // Insertion check 1: When text is null the call has no effect.
    reset();
    try
      {
        doc.insertString(8888, null, SimpleAttributeSet.EMPTY);
      }
    catch (BadLocationException ble)
      {
        testException = ble;
	harness.debug(ble);
      }

    harness.check(testException, null);
    harness.check(testOffset, DEFAULT_INT);
    harness.check(testAttr, DEFAULT_ATTRIBUTESET);
    harness.check(testWhere, DEFAULT_INT);
    harness.check(testNitems, DEFAULT_INT);

    // Insertion check 2: When text is "" the call has no effect.
    reset();
    try
      {
        doc.insertString(8888, "", SimpleAttributeSet.EMPTY);
      }
    catch (BadLocationException ble)
      {
        testException = ble;
	harness.debug(ble);
      }

    harness.check(testException, null);
    harness.check(testOffset, DEFAULT_INT);
    harness.check(testAttr, DEFAULT_ATTRIBUTESET);
    harness.check(testWhere, DEFAULT_INT);
    harness.check(testNitems, DEFAULT_INT);

    harness.checkPoint("replace");

    // Replacement check 1: When length is zero and text is null the
    // call has no effect.
    reset();
    try
      {
        doc.replace(8888, 0, null, SimpleAttributeSet.EMPTY);
      }
    catch (BadLocationException ble)
      {
        testException = ble;
	harness.debug(ble);
      }

    harness.check(testException, null);
    harness.check(testOffset, DEFAULT_INT);
    harness.check(testLength, DEFAULT_INT);
    harness.check(testAttr, DEFAULT_ATTRIBUTESET);
    harness.check(testString, DEFAULT_STRING);
    harness.check(testWhere, DEFAULT_INT);
    harness.check(testNitems, DEFAULT_INT);

    // Replacement check 2: When length is zero and text is "" the
    // call has no effect.
    reset();
    try
      {
        doc.replace(8888, 0, "", SimpleAttributeSet.EMPTY);
      }
    catch (BadLocationException ble)
      {
        testException = ble;
      }

    harness.check(testException, null);
    harness.check(testOffset, DEFAULT_INT);
    harness.check(testLength, DEFAULT_INT);
    harness.check(testAttr, DEFAULT_ATTRIBUTESET);
    harness.check(testString, DEFAULT_STRING);
    harness.check(testWhere, DEFAULT_INT);
    harness.check(testNitems, DEFAULT_INT);

    // Replacement check 4: When length is non-zero (even negative)
    // and text is null the call has is forwarded to the DocumentFilter
    // but not to the GapContent.
    reset();
    try
      {
        doc.replace(0, -1, null, SimpleAttributeSet.EMPTY);
      }
    catch (BadLocationException ble)
      {
        testException = ble;
	harness.debug(ble);
      }

    harness.check(testException, null);
    harness.check(testOffset, 0);
    harness.check(testLength, -1);
    harness.check(testAttr, SimpleAttributeSet.EMPTY);
    harness.check(testString, null);
    harness.check(testWhere, DEFAULT_INT);
    harness.check(testNitems, DEFAULT_INT);

    // Replacement check 5: When length is non-zero (even negative)
    // and text is "" the call has is forwarded to the DocumentFilter
    // but not to the GapContent.
    reset();
    try
      {
        doc.replace(0, -1, "", SimpleAttributeSet.EMPTY);
      }
    catch (BadLocationException ble)
      {
        testException = ble;
	harness.debug(ble);
      }

    harness.check(testException, null);
    harness.check(testOffset, 0);
    harness.check(testLength, -1);
    harness.check(testAttr, SimpleAttributeSet.EMPTY);
    harness.check(testString, "");
    harness.check(testWhere, DEFAULT_INT);
    harness.check(testNitems, DEFAULT_INT);

    // Replacement check 6: When length is zero and text has a length > 0
    // the call has is forwarded to the DocumentFilter and the GapContent.
    reset();
    try
      {
        doc.replace(10, 0, "FOO!", SimpleAttributeSet.EMPTY);
      }
    catch (BadLocationException ble)
      {
        testException = ble;
	harness.debug(ble);
      }

    harness.check(testException, null);
    harness.check(testOffset, 10);
    harness.check(testLength, 0);
    harness.check(testAttr, SimpleAttributeSet.EMPTY);
    harness.check(testString, "FOO!");
    harness.check(testWhere, DEFAULT_INT);
    harness.check(testNitems, DEFAULT_INT);

    // Note: It was expected that the call to replace() would result
    // in a call to GapContent.insertString() but that could not be
    // observed.
    // The test therefore looks whether the string has been properly
    // inserted.
    try
      {
        harness.check(doc.getText(10, 4), "FOO!");
      }
    catch(BadLocationException ble)
     {
       harness.debug(ble);
       harness.fail("replace6");
     }

  }

  void reset()
  {
    testOffset = DEFAULT_INT;
    testLength = DEFAULT_INT;
    testString = DEFAULT_STRING;
    testAttr = DEFAULT_ATTRIBUTESET;
    testException = null;

    testWhere = DEFAULT_INT;
    testNitems = DEFAULT_INT;
    testInsertWhere = DEFAULT_INT;
  }

        // An inner class which simply copies the argument values to fields of the outer class
        // for later inspection.
	class TestDocFilter extends DocumentFilter
	{

	  public void insertString(DocumentFilter.FilterBypass fb, int offset, String string, AttributeSet attr) throws BadLocationException
          {
	    testOffset = offset;
            testString = string;
            testAttr = attr;
            super.insertString(fb, offset, string, attr);
	  }

          public void remove(DocumentFilter.FilterBypass fb, int offset, int length) throws BadLocationException
          {
	    testOffset = offset;
            testLength = length;
            super.remove(fb, offset, length);
          }

          public void replace(DocumentFilter.FilterBypass fb, int offset, int length, String string, AttributeSet attr) throws BadLocationException
          {
	    testOffset = offset;
            testLength = length;
            testString = string;
            testAttr = attr;
            super.replace(fb, offset, length, string, attr);
          }

	}

  class TestGapContent extends GapContent
  {

    public UndoableEdit insertString(int where, String str) throws BadLocationException
    {
      testInsertWhere = where;

      return super.insertString(where, str);
    }

    public UndoableEdit remove(int where, int nitems) throws BadLocationException
    {
      testWhere = where;
      testNitems = nitems;

      return super.remove(where, nitems);
    }
  }
}
