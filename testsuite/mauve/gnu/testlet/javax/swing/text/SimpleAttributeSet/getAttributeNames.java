/* getAttributeNames.java -- Some checks for the getAttributeNames() method in 
                             the SimpleAttributeSet class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.text.SimpleAttributeSet;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Enumeration;

import javax.swing.text.AttributeSet;
import javax.swing.text.SimpleAttributeSet;

public class getAttributeNames implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("getAttributeNames()");
      
    SimpleAttributeSet s = new SimpleAttributeSet();
    Enumeration e = s.getAttributeNames();
    harness.check(e.hasMoreElements(), false);

    s.addAttribute("X1", "Y1");
    e = s.getAttributeNames();
    harness.check(e.hasMoreElements(), true);
    harness.check(e.nextElement(), "X1");
    harness.check(e.hasMoreElements(), false);
      
    // add an attribute that exists in the resolve parent, same value
    SimpleAttributeSet sParent = new SimpleAttributeSet();
    s.setResolveParent(sParent);
    s.removeAttribute("X1");
    e = s.getAttributeNames();
    harness.check(e.hasMoreElements(), true);
    harness.check(e.nextElement(), AttributeSet.ResolveAttribute);
    harness.check(e.hasMoreElements(), false);
  }
}