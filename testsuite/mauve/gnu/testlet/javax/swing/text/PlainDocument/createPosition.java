/* createPosition.java -- Some checks for the createPosition() method in the 
                          PlainDocument class.
   Copyright (C) 2006  David Gilbert <david.gilbert@object-refinery.com>
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
import javax.swing.text.PlainDocument;
import javax.swing.text.Position;

public class createPosition implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    test1(harness);
    test2(harness);
  }
  
  public void test1(TestHarness harness)
  {
    harness.checkPoint("test1");
    PlainDocument d = new PlainDocument();
    try
    {
      Position p0 = d.createPosition(0);
      harness.check(p0.getOffset(), 0);
      Position p1 = d.createPosition(1);
      harness.check(p1.getOffset(), 1);
      d.insertString(0, "ABC", null);
      harness.check(p0.getOffset(), 0);
      harness.check(p1.getOffset(), 4);
    }
    catch (BadLocationException e)
    { 
    }
  }

  public void test2(TestHarness harness)
  {
    harness.checkPoint("test2");
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
  
}