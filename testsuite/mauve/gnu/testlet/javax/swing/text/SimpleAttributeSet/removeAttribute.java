/* removeAttribute.java -- Tests for the removeAttribute() method in the 
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
 * Some checks for the removeAttribute() method in the 
 * {@link SimpleAttributeSet} class.
 */
public class removeAttribute implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("removeAttribute()");
    
    SimpleAttributeSet s = new SimpleAttributeSet();
    // remove an attribute that doesn't exist
    s.removeAttribute("X1");
    s.addAttribute("X1", "Y1");
    harness.check(s.getAttribute("X1"), "Y1");
    // remove another attribute that doesn't exist
    s.removeAttribute("X2");
    harness.check(s.getAttributeCount(), 1);
    // remove an existing attribute
    s.removeAttribute("X1");
    harness.check(s.getAttribute("X1"), null);
    
    SimpleAttributeSet sParent = new SimpleAttributeSet();
    sParent.addAttribute("XX", "YY");
    s.setResolveParent(sParent);
    harness.check(sParent.getAttributeCount(), 1);
    s.removeAttribute("XX");
    harness.check(sParent.getAttribute("XX"), "YY");
    
    // try null attribute key
    boolean pass = false;
    try
      {
        s.removeAttribute(null);
      }
    catch (NullPointerException e)
      {
        pass = true;  
      }
    harness.check(pass);    
  }

}