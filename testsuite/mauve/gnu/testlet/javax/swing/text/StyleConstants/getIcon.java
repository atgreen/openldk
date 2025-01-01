/* getIcon.java -- Tests for the getIcon() method in the StyleConstants class.
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

import javax.swing.Icon;
import javax.swing.plaf.metal.MetalIconFactory;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

/**
 * Some checks for the getIcon() method in the {@link StyleConstants} class.
 */
public class getIcon implements Testlet
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
    harness.check(StyleConstants.getIcon(s), null);
    
    // check local setting
    Icon icon1 = MetalIconFactory.getFileChooserHomeFolderIcon();
    StyleConstants.setIcon(s, icon1);
    harness.check(StyleConstants.getIcon(s), icon1);
    
    // check resolving parent setting
    s.removeAttribute(StyleConstants.IconAttribute);
    SimpleAttributeSet parent = new SimpleAttributeSet();
    s.setResolveParent(parent);
    Icon icon2 = MetalIconFactory.getFileChooserNewFolderIcon();
    StyleConstants.setIcon(parent, icon2);
    harness.check(StyleConstants.getIcon(s), icon2);    
    
    // try null argument
    boolean pass = false;
    try
      {
        StyleConstants.getIcon(null);
      }
    catch (NullPointerException e)
      {
        pass = true;  
      }
    harness.check(pass);    
  }

}