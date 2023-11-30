/* addAttributes.java -- Tests for the addAttributes() method in the 
                         SimpleAttributeSet class.
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

import javax.swing.text.SimpleAttributeSet;

/**
 * Some checks for the addAttributes() method in the {@link SimpleAttributeSet} 
 * class.
 */
public class addAttributes implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("addAttributes()");
    
    // try an empty set
    SimpleAttributeSet s = new SimpleAttributeSet();
    s.addAttributes(SimpleAttributeSet.EMPTY);
    harness.check(s.isEmpty());
    
    // try a set with no resolve parent and a couple of attributes
    SimpleAttributeSet atts1 = new SimpleAttributeSet();
    atts1.addAttribute("A1", "B1");
    atts1.addAttribute("A2", "B2");
    s.addAttributes(atts1);
    harness.check(s.getAttributeCount(), 2);
    harness.check(s.containsAttribute("A1", "B1"));
    harness.check(s.containsAttribute("A2", "B2"));
    
    // try a set with a resolve parent
    SimpleAttributeSet atts2 = new SimpleAttributeSet();
    atts2.addAttribute("C1", "D1");
    atts1.addAttribute("A1", "BB1");
    atts1.addAttribute("A2", "BB2");
    atts1.setResolveParent(atts2);
    s.addAttributes(atts1);
    harness.check(s.getResolveParent(), atts2);
    harness.check(s.getAttributeCount(), 3);
    harness.check(s.containsAttribute("A1", "BB1"));
    harness.check(s.containsAttribute("A2", "BB2"));
    harness.check(s.containsAttribute("C1", "D1"));
    
    // try a null set
    boolean pass = false;
    try
      {
        s.addAttributes(null);
      }
    catch (NullPointerException e)
      {
        pass = true;  
      }
    harness.check(pass);
    
  }

}