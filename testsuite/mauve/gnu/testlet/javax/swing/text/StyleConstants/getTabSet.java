/* getTabSet.java -- Tests for the getTabSet() method in the 
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
import javax.swing.text.TabSet;
import javax.swing.text.TabStop;

/**
 * Some checks for the getTabSet() method in the {@link StyleConstants} 
 * class.
 */
public class getTabSet implements Testlet
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
    harness.check(StyleConstants.getTabSet(s), null);
    
    // check local setting
    TabStop[] tabs1 = new TabStop[] {new TabStop(8.0f)};
    TabSet ts1 = new TabSet(tabs1);
    StyleConstants.setTabSet(s, ts1);
    harness.check(StyleConstants.getTabSet(s), ts1);
    
    // check resolving parent setting
    s.removeAttribute(StyleConstants.TabSet);
    SimpleAttributeSet parent = new SimpleAttributeSet();
    s.setResolveParent(parent);
    TabStop[] tabs2 = new TabStop[] {new TabStop(10.0f)};
    TabSet ts2 = new TabSet(tabs2);
    StyleConstants.setTabSet(parent, ts2);
    harness.check(StyleConstants.getTabSet(s), ts2);    
    
    // try null argument
    boolean pass = false;
    try
      {
        StyleConstants.getTabSet(null);
      }
    catch (NullPointerException e)
      {
        pass = true;  
      }
    harness.check(pass);    
  }

}