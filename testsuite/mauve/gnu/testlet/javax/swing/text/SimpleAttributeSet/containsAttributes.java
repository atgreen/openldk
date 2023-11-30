/* containsAttributes.java -- Tests for the containsAttributes() method in the 
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
 * Some checks for the containsAttributes() method in the 
 * {@link SimpleAttributeSet} class.
 */
public class containsAttributes implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("containsAttributes()");
    
    // start with an empty set
    SimpleAttributeSet s = new SimpleAttributeSet();
    
    SimpleAttributeSet atts = new SimpleAttributeSet();
    harness.check(s.containsAttributes(atts), true);
    
    atts.addAttribute("E", "5");
    harness.check(s.containsAttributes(atts), false);
    
    // now populate the set and run some more tests
    s.addAttribute("A", "1");    
    s.addAttribute("B", "2");    
    s.addAttribute("C", "3");    
    s.addAttribute("D", "4");    
    s.addAttribute("E", "5");    
    s.addAttribute("F", "6");
    
    harness.check(s.containsAttributes(atts), true);
    atts.addAttribute("E", "XXX");
    harness.check(s.containsAttributes(atts), false);
    atts.removeAttribute("E");  // atts is now empty
    harness.check(s.containsAttributes(atts), true);

    atts.addAttribute("A", "1");
    atts.addAttribute("D", "4");
    atts.addAttribute("E", "5");
    harness.check(s.containsAttributes(atts), true);
    atts.addAttribute("D", "XXX");
    harness.check(s.containsAttributes(atts), false);
    
    // now do some checks on the resolving parent
    s = new SimpleAttributeSet();
    SimpleAttributeSet sParent = new SimpleAttributeSet();
    s.setResolveParent(sParent);
    
    atts = new SimpleAttributeSet();
    harness.check(s.containsAttributes(atts), true);
    atts.setResolveParent(sParent);
    harness.check(s.containsAttributes(atts), true);

    sParent.addAttribute("X", "1");
    atts.addAttribute("X", "1");
    harness.check(s.containsAttributes(atts));
    
    // try null attributes
    boolean pass = false;
    try
      {
        s.containsAttributes(null);
      }
    catch (NullPointerException e)
      {
        pass = true;  
      }
    harness.check(pass);    
  }

}