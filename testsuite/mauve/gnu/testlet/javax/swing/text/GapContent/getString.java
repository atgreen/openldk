/* getString.java -- Some checks for the getString() method in the GapContent
                     class.
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

package gnu.testlet.javax.swing.text.GapContent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.BadLocationException;
import javax.swing.text.GapContent;

public class getString implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    GapContent gc = new GapContent();
    
    // check default result
    boolean pass = false;
    try
    {
      pass = gc.getString(0, 1).equals("\n");
    }
    catch (BadLocationException e)
    {
    }
    harness.check(pass);

    // if len goes past end of range, should get BadLocationException
    pass = false;
    try
    {
      gc.getString(0, 2);
    }
    catch (BadLocationException e)
    {
      pass = true;
    }
    harness.check(pass);

    // add some more text
    try
    {
      gc.insertString(0, "ABCDEFG");
    }
    catch (BadLocationException e)
    {
    }
    harness.check(gc.length(), 8);
    
    // if index < 0 should get BadLocationException
    pass = false;
    try
    {
      /*String s =*/ gc.getString(-1, 3);
    }
    catch (StringIndexOutOfBoundsException e) 
    {
      pass = false;  // JDK does this, API docs say it should be a 
                     // BadLocationException
    }
    catch (BadLocationException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // if index > end of text should get BadLocationException
    pass = false;
    try
    {
      /*String s =*/ gc.getString(99, 1);
    }
    catch (BadLocationException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // if len goes past end of range, should get BadLocationException
    pass = false;
    try
    {
      /* String s =*/ gc.getString(0, 99);
    }
    catch (BadLocationException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try a zero length string
    pass = false;
    try
    {
      pass = gc.getString(1, 0).equals("");
    }
    catch (BadLocationException e)
    {
    }
    harness.check(pass);
    
  }

}