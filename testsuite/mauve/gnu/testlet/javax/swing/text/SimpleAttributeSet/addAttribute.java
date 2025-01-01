/* addAttribute.java -- Tests for the addAttribute() method in the 
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
 * Some checks for the addAttribute() method in the {@link SimpleAttributeSet} 
 * class.
 */
public class addAttribute implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("addAttribute()");
    
    SimpleAttributeSet s = new SimpleAttributeSet();
    
    // add an attribute, check that you can retrieve it
    s.addAttribute("X1", "Y1");
    harness.check(s.getAttribute("X1"), "Y1");
    
    // add an attribute that already exists, same value
    s.addAttribute("X1", "Y1");
    harness.check(s.getAttributeCount(), 1);
    
    // add an attribute that already exists, different value
    s.addAttribute("X1", "YY1");
    harness.check(s.getAttribute("X1"), "YY1");
    harness.check(s.getAttributeCount(), 1);
    
    // add an attribute that exists in the resolve parent, same value
    SimpleAttributeSet sParent = new SimpleAttributeSet();
    sParent.addAttribute("X2", "Y2");
    s.setResolveParent(sParent);
    harness.check(s.getAttributeCount(), 2);
    
    // this adds to s, not sParent
    s.addAttribute("X2", "Y2");
    harness.check(s.getAttribute("X2"), "Y2");
    harness.check(s.getAttributeCount(), 3);
    
    // add an attribute that exists in the resolve parent, different value
    s.addAttribute("X2", "Z2");
    harness.check(s.getAttribute("X2"), "Z2");
    harness.check(s.getAttributeCount(), 3);
    harness.check(sParent.getAttribute("X2"), "Y2");
    harness.check(sParent.getAttributeCount(), 1);
    
    // try null attribute key
    boolean pass = false;
    try
      {
        s.addAttribute(null, "XX");
      }
    catch (NullPointerException e)
      {
        pass = true;  
      }
    harness.check(pass);
    
    // try null value
    pass = false;
    try
      {
        s.addAttribute("X2", null);
      }
    catch (NullPointerException e) 
      {
        pass = true;  
      }
    harness.check(pass);  
    
  }

}