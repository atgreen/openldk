/* setTabSet.java -- Tests for the setTabSet() method in the StyleConstants 
                     class.
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
 * Some checks for the setTabSet() method in the {@link StyleConstants} 
 * class.
 */
public class setTabSet implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    SimpleAttributeSet s = new SimpleAttributeSet();
    
    // check local setting
    TabStop[] tabs1 = new TabStop[] {new TabStop(8.0f)};
    TabSet ts1 = new TabSet(tabs1);
    StyleConstants.setTabSet(s, ts1);
    harness.check(StyleConstants.getTabSet(s), ts1);
    
    // try null value
    boolean pass = false;
    try
      {
        StyleConstants.setTabSet(s, null);
      }
    catch (NullPointerException e)
      {
        pass = true;  
      }
    harness.check(pass);    

    // try null key
    pass = false;
    try
      {
        StyleConstants.setTabSet(null, ts1);
      }
    catch (NullPointerException e)
      {
        pass = true;  
      }
    harness.check(pass);    
  }

}