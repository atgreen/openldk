/* getBidiLevel.java -- Tests for the getBidiLevel() method in the 
                        StyleConstants class.
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

package gnu.testlet.javax.swing.text.StyleConstants;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

/**
 * Some checks for the getBidiLevel() method in the {@link StyleConstants} 
 * class.
 */
public class getBidiLevel implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    SimpleAttributeSet s = new SimpleAttributeSet();
    
    // check default
    harness.check(StyleConstants.getBidiLevel(s), 0);
    
    // check local setting
    StyleConstants.setBidiLevel(s, 1);
    harness.check(StyleConstants.getBidiLevel(s), 1);
    
    // check resolving parent setting
    s.removeAttribute(StyleConstants.BidiLevel);
    SimpleAttributeSet parent = new SimpleAttributeSet();
    s.setResolveParent(parent);
    StyleConstants.setBidiLevel(parent, 2);
    harness.check(StyleConstants.getBidiLevel(s), 2);    
    
    // try null argument
    boolean pass = false;
    try
      {
        StyleConstants.getBidiLevel(null);
      }
    catch (NullPointerException e)
      {
        pass = true;  
      }
    harness.check(pass);    
  }

}