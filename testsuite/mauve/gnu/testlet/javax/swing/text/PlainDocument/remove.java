/* remove.java -- Some checks for the remove() method in the PlainDocument 
   class.
   Copyright (C) 2006  David Gilbert <david.gilbert@object-refinery.com>
   Copyright (C) 2006  Robert Schuster <robertschuster@fsfe.org>
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

// Tags: 1.2

package gnu.testlet.javax.swing.text.PlainDocument;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.PlainDocument;
import javax.swing.text.Position;

public class remove implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testArguments(harness);
    testPositions(harness);

    testBehavior(harness);
  }
  
  public void testArguments(TestHarness harness)
  {
    harness.checkPoint("testArguments");
    PlainDocument d = new PlainDocument();
    try
    {
      d.insertString(0, "ABC", null);
    }
    catch (BadLocationException e)
    { 
    }
    
    // negative index
    boolean pass = false;
    try
    {
      d.remove(-1, 1);
    }
    catch (BadLocationException e)
    { 
      pass = true;
    }
    harness.check(pass);
    
    // index too high
    pass = false;
    try
    {
      d.remove(4, 0);
    }
    catch (BadLocationException e)
    { 
      pass = true;
    }
    harness.check(pass);
    
    // index + length to high
    pass = false;
    try
    {
      d.remove(2, 99);
    }
    catch (BadLocationException e)
    { 
      pass = true;
    }
    harness.check(pass);

    // negative length
    pass = false;
    try
    {
      d.remove(2, -1);
    }
    catch (BadLocationException e)
    { 
      pass = true;
    }
    harness.check(pass);
  
  }

  public void testPositions(TestHarness harness)
  {
    harness.checkPoint("testPositions");
    PlainDocument d = new PlainDocument();
    try
    {
      d.insertString(0, "ABCDEF", null);
      Position p0 = d.createPosition(0);
      harness.check(p0.getOffset(), 0);
      Position p1 = d.createPosition(1);
      harness.check(p1.getOffset(), 1);
      Position p2 = d.createPosition(2);
      harness.check(p2.getOffset(), 2);
      Position p3 = d.createPosition(3);
      harness.check(p3.getOffset(), 3);
      Position p4 = d.createPosition(4);
      harness.check(p4.getOffset(), 4);
      Position p5 = d.createPosition(5);
      harness.check(p5.getOffset(), 5);
      Position p6 = d.createPosition(6);
      harness.check(p6.getOffset(), 6);
      
      d.remove(2, 2);
      harness.check(p0.getOffset(), 0);
      harness.check(p1.getOffset(), 1);
      harness.check(p2.getOffset(), 2);
      harness.check(p3.getOffset(), 2);      
      harness.check(p4.getOffset(), 2);      
      harness.check(p5.getOffset(), 3);      
      harness.check(p6.getOffset(), 4);
      
      d.remove(0, 1);
      harness.check(p0.getOffset(), 0);
      harness.check(p1.getOffset(), 0);
      harness.check(p2.getOffset(), 1);
      harness.check(p3.getOffset(), 1);      
      harness.check(p4.getOffset(), 1);      
      harness.check(p5.getOffset(), 2);      
      harness.check(p6.getOffset(), 3);
      
    }
    catch (BadLocationException e)
    { 
    }
  }
  
  private void testBehavior(TestHarness harness)
  {
    harness.checkPoint("testArguments");

    PlainDocument pd = new PlainDocument();
    int index0 = -1;
    int index1 = -1;

    try
      {
        pd.insertString(0, "abc\ndef\n", null);

        // Delete the 'c'.
        pd.remove(2, 1);

        Element re = pd.getDefaultRootElement();
        
        // There should still be two separate lines
        // (no Element merge should have taken place).
        index0 = re.getElementIndex(2);
        index1 = re.getElementIndex(3);
      }
    catch (BadLocationException ble)
      {
        // Very odd. This should not happen.
        index0 = -1;
        index1 = -1;
      }
    finally
      {
        harness.check(index0, 0);
        harness.check(index1, 1);
      }
    
  }
}
