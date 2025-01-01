/* createPosition.java -- Some checks for the createPosition() method in the 
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
import javax.swing.text.Position;
import javax.swing.text.StringContent;

public class createPosition implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    StringContent sc = new StringContent();
    harness.check(sc.length(), 1);
    try
    {
      sc.insertString(0, "ABC");
    }
    catch (BadLocationException e) 
    {
      // ignore
    }
    
    // negative index
    boolean pass = false;
    try
    {
      sc.createPosition(-1);
    }
    catch (BadLocationException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // index of last char
    Position p = null;
    try
    {
      p = sc.createPosition(3);
    }
    catch (BadLocationException e)
    {
      pass = true;
    }
    harness.check(p.getOffset(), 3);
    
    // index of last char + 1
    try
    {
      p = sc.createPosition(4);
    }
    catch (BadLocationException e)
    {
    }
    harness.check(p.getOffset(), 4);
    
    // index of last char + 2
    pass = false;
    try
    {
      p = sc.createPosition(5);
    }
    catch (BadLocationException e)
    {
      pass = true;
    }
    harness.check(pass);
    
  }

}