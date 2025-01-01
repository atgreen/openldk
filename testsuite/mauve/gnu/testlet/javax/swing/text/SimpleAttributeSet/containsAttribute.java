/* containsAttribute.java -- Tests for the containsAttribute() method in the 
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
 * Some checks for the containsAttribute() method in the 
 * {@link SimpleAttributeSet} class.
 */
public class containsAttribute implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("containsAttribute()");
    
    SimpleAttributeSet s = new SimpleAttributeSet();
    harness.check(s.containsAttribute("X1", "Y1"), false);
    s.addAttribute("X1", "Y1");
    harness.check(s.containsAttribute("X1", "Y1"), true);
    harness.check(s.containsAttribute("X1", "Y2"), false);
    harness.check(s.containsAttribute("X2", "Y1"), false);
        
    // check with resolve parent
    SimpleAttributeSet sParent = new SimpleAttributeSet();
    sParent.addAttribute("X2", "Y2");
    s.setResolveParent(sParent);
    harness.check(s.containsAttribute("X1", "Y1"), true);
    harness.check(s.containsAttribute("X2", "Y2"), true);
    s.addAttribute("X2", "ZZ");
    harness.check(s.containsAttribute("X2", "ZZ"), true);
    harness.check(s.containsAttribute("X2", "Y2"), false);
    
    // try null attribute key
    boolean pass = false;
    try
      {
        s.containsAttribute(null, "XX");
      }
    catch (NullPointerException e)
      {
        pass = true;  
      }
    harness.check(pass);
    
    // try null value for existing key
    pass = false;
    try
      {
        s.containsAttribute("X2", null);
      }
    catch (NullPointerException e) 
      {
        pass = true;  
      }
    harness.check(pass);  
    
    // try null value for non-existent key
    pass = false;
    try
      {
        s.containsAttribute("XXX", null);
      }
    catch (NullPointerException e) 
      {
        pass = true;  
      }
    harness.check(pass);  
    
  }

}