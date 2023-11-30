/* insertString.java -- Some checks for the insertString() method in the 
                        StringContent class.
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

package gnu.testlet.javax.swing.text.StringContent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.BadLocationException;
import javax.swing.text.StringContent;

public class insertString implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    StringContent sc = new StringContent();
    // regular insert
    try
    {
      sc.insertString(0, "ABC");
      // ignoring undo/redo here - see insertUndo.java
    }
    catch (BadLocationException e)
    {
      // ignore - checks below will fail if this happens
    }
    harness.check(sc.length(), 4);
    
    // insert at location before start
    boolean pass = false;
    try
    {
      sc.insertString(-1, "XYZ");
    }
    catch (BadLocationException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // insert at index of last character - this is OK
    try
    {
      sc.insertString(3, "XYZ");
    }
    catch (BadLocationException e)
    {
      // ignore
    }
    harness.check(sc.length(), 7);
    
    // insert at index of last character + 1 - this raises BadLocationException
    pass = false;
    try
    {
      sc.insertString(7, "XYZ");
    }
    catch (BadLocationException e)
    {
      pass = true;
    }
    harness.check(pass);

    // insert empty string
    try
    {
      sc.insertString(0, "");
    }
    catch (BadLocationException e)
    {
      // ignore
    }
    harness.check(sc.length(), 7);
    
    // insert null string
    pass = false;
    try
    {
      sc.insertString(0, null);
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

}