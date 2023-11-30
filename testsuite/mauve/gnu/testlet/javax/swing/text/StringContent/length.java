/* length.java -- Some checks for the length() method in the StringContent 
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

package gnu.testlet.javax.swing.text.StringContent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.BadLocationException;
import javax.swing.text.StringContent;

public class length implements Testlet
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
      sc.insertString(0, "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
      harness.check(sc.length(), 27);
      sc.remove(0, 3);
      harness.check(sc.length(), 24);
      sc.insertString(4, "123");
      harness.check(sc.length(), 27);
      sc.remove(20, 5);
      harness.check(sc.length(), 22);  
    }
    catch (BadLocationException e) 
    {
      harness.fail(e.toString());  
    }
  }

}