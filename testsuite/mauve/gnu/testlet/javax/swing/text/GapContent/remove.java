/* remove.java -- Some checks for the remove() method in the 
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

package gnu.testlet.javax.swing.text.GapContent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.BadLocationException;
import javax.swing.text.GapContent;
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
    testGeneral(harness);
    testRemoveLast(harness);
  }
  
  public void testGeneral(TestHarness harness) 
  {
    GapContent gc = new GapContent();
    // regular insert
    try
    {
      gc.insertString(0, "ABCDEFG");
      // ignoring undo/redo here 
    }
    catch (BadLocationException e)
    {
      // ignore - checks below will fail if this happens
    }
    harness.check(gc.length(), 8);
    
    // remove from location before start
    boolean pass = false;
    try
    {
      gc.remove(-1, 3);
    }
    catch (BadLocationException e)
    {
      pass = true;  // Classpath does this
    }
    catch (StringIndexOutOfBoundsException e) 
    {
      pass = false;  // JDK does this - it is a bug given the API spec
    }
    harness.check(pass);
    
    // remove from location after end
    pass = false;
    try
    {
      gc.remove(99, 1);
    }
    catch (BadLocationException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // doesn't allow removal of last char
    pass = false;
    try
    {
      gc.remove(7, 1);
    }
    catch (BadLocationException e)
    {
      pass = true;
    }
    harness.check(pass);
    harness.check(gc.length(), 8);

    // remove 0 chars
    pass = true;
    try
    {
      gc.remove(0, 0);
    }
    catch (BadLocationException e)
    {
      pass = false;
    }
    harness.check(pass);
    harness.check(gc.length(), 8);
    

    int offset = 0;
    try
    {
      gc = new GapContent();
      gc.insertString(0, "abc\ndef\n");

      // create position on the 'd'.
      Position pos = gc.createPosition(4);

      // remove the 'd'
      gc.remove(4, 1);

      offset = pos.getOffset();
    }
    catch(BadLocationException ble)
    {
      // If that happens something is pretty odd
      offset = -1;
    }
    finally
    {
      // offset of our position should *NOT* have changed
      harness.check(offset, 4);
    }

  }
  
  /**
   * The API spec says that the last character cannot be removed
   * (where + nitems < length()).
   * 
   * @param harness
   */
  public void testRemoveLast(TestHarness harness) 
  {
    harness.checkPoint("testRemoveLast");
    GapContent gc = new GapContent();
    harness.check(gc.length(), 1);
    
    boolean pass = false;
    try
    {
      gc.remove(0, 1);
    }
    catch (BadLocationException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

}

