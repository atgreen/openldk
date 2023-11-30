// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>
// Copyright (C) 2006 David Gilbert  <david.gilbert@object-refinery.com>
// Copyright (C) 2006 Robert Schuster  <robertschuster@fsfe.org>

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

package gnu.testlet.javax.swing.text.GapContent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.BadLocationException;
import javax.swing.text.GapContent;
import javax.swing.text.Position;

/**
 * Tests if GapContent.insertString works correctly.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class insertString implements Testlet
{

  /**
   * A subclass of GapContent that provides access to the protected methods.
   */
  class TestGapContent extends GapContent
  {
    public int getArrayLength()
    {
      return super.getArrayLength();
    }
    public int getTestGapStart()
    {
      return super.getGapStart();
    }
    public int getTestGapEnd()
    {
      return super.getGapEnd();
    }
  }

  /**
   * Start the test run.
   * 
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    testSmallInsert(harness);
    testBiggerInsert(harness);
    testBigInsert(harness);
    testComplexInsert(harness);
    testGeneral(harness);

    testSpecialInsert(harness);
  }

  /**
   * Tests the insertion of a small (< gapsize) chunk of text.
   *
   * @param harness the test harness to use
   */
  void testSmallInsert(TestHarness harness)
  {
    harness.checkPoint("smallInsert");
    try
      {
        TestGapContent c = new TestGapContent();
        checkBuffer(harness, c, 10, 1, 10);
        c.insertString(0, "abcdefgh");
        checkBuffer(harness, c, 10, 8, 9);
      }
    catch (BadLocationException ex)
      {
        harness.fail("BadLocationException thrown.");
      }
  }

  /**
   * Tests the insertion of a bigger (= gapsize) chunk of text.
   *
   * @param harness the test harness to use
   */
  void testBiggerInsert(TestHarness harness)
  {
    harness.checkPoint("biggerInsert");
    try
      {
        TestGapContent c = new TestGapContent();
        checkBuffer(harness, c, 10, 1, 10);
        c.insertString(0, "abcdefghi");
        checkBuffer(harness, c, 22, 9, 21);
      }
    catch (BadLocationException ex)
      {
        harness.fail("BadLocationException thrown.");
      }
  }

  /**
   * Tests the insertion of a bigger (= gapsize) chunk of text.
   *
   * @param harness the test harness to use
   */
  void testBigInsert(TestHarness harness)
  {
    harness.checkPoint("bigInsert");
    try
      {
        TestGapContent c = new TestGapContent();
        checkBuffer(harness, c, 10, 1, 10);
        c.insertString(0, "abcdefghijkl");
        checkBuffer(harness, c, 28, 12, 27);
      }
    catch (BadLocationException ex)
      {
        harness.fail("BadLocationException thrown.");
      }
  }

  /**
   * Tests a more complex insertion.
   *
   * @param harness the test harness to use
   */
  void testComplexInsert(TestHarness harness)
  {
    harness.checkPoint("complexInsert");
    try
      {
        TestGapContent c = new TestGapContent();
        checkBuffer(harness, c, 10, 1, 10);
        c.insertString(0, "abcdefghijklmno");
        checkBuffer(harness, c, 34, 15, 33);
        c.insertString(5, "12345");
        checkBuffer(harness, c, 34, 10, 23);
      }
    catch (BadLocationException ex)
      {
        harness.fail("BadLocationException thrown.");
      }
  }

  /**
   * Checks the state of the GapContent's buffer array.
   *
   * @param harness the test harness to use
   * @param c the gap content to check
   * @param arrayLength the expected array length
   * @param gapStart the expected gap start
   * @param gapEnd the expected gap end
   */
  void checkBuffer(TestHarness harness, TestGapContent c, int arrayLength,
                   int gapStart, int gapEnd)
  {
    harness.check(c.getArrayLength(), arrayLength);
    harness.check(c.getTestGapStart(), gapStart);
    harness.check(c.getTestGapEnd(), gapEnd);
  }
  
  /**
   * The same tests that I added for StringContent.java.
   */
  public void testGeneral(TestHarness harness)      
  {
    harness.checkPoint("testGeneral()");
    
    GapContent gc = new GapContent();
    // regular insert
    try
    {
      gc.insertString(0, "ABC");
      // ignoring undo/redo here - see insertUndo.java
    }
    catch (BadLocationException e)
    {
      // ignore - checks below will fail if this happens
    }
    harness.check(gc.length(), 4);
    
    // insert at location before start
    boolean pass = false;
    try
    {
      gc.insertString(-1, "XYZ");
    }
    catch (BadLocationException e)
    {
      pass = true;
    }
    catch (Exception _)
    {
      pass = false;
    }

    harness.check(pass);
    
    // insert at index of last character - this is OK
    try
    {
      gc.insertString(3, "XYZ");
    }
    catch (BadLocationException e)
    {
      // ignore
    }
    harness.check(gc.length(), 7);
    
    // Insert at index of last character + 1. This appends the new string to
    // the existing string. (This works since 1.5 of the RI)
    try
    {
      gc.insertString(7, "XYZ");
    }
    catch (BadLocationException e)
    {
      pass = false;
    }
    harness.check(gc.length(), 10);

    // insert at index of last character + 2 
    pass = false;
    try
    {
      gc.insertString(gc.length() + 1, "XYZ");
    }
    catch (BadLocationException e)
    {
      pass = true;
    }
    harness.check(pass);

    // insert empty string
    int length = gc.length();
    try
    {
      gc.insertString(0, "");
    }
    catch (BadLocationException e)
    {
      // ignore
    }
    harness.check(gc.length(), length);
    
    // insert null string
    pass = false;
    try
    {
      gc.insertString(0, null);
    }
    catch (BadLocationException e)
    {
      // ignore
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);    
  }

  public void testSpecialInsert(TestHarness harness)
    {
      harness.checkPoint("specialInsert");
      int posValue = -1;

      try
        {
	  GapContent gc = new GapContent();
  	  gc.insertString(0, "foo\nbar\n");
	  gc.remove(0, 4);

          // Gap starts at 0 now and the following Position
          // instance is created at the end of the gap.
          Position pos = gc.createPosition(0);

          // This insertion should not move the offset
          // of the Position object.
	  gc.insertString(0, "z");

	  posValue = pos.getOffset();
        }
      catch(BadLocationException ble)
        {
        }
      harness.check(posValue, 0);
    }


}
