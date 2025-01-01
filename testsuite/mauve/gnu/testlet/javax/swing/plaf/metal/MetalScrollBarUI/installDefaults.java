/* installDefaults.java -- some checks for the installDefaults() method in the
       MetalScrollBarUI class.
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

// Tags: JDK1.5
// Uses: MyMetalScrollBarUI

package gnu.testlet.javax.swing.plaf.metal.MetalScrollBarUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JScrollBar;
import javax.swing.UIManager;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some tests for the initialisation performed in the installDefaults() method.
 */
public class installDefaults implements Testlet
{
  
  public void test(TestHarness harness)
  {
    // use a known look and feel
    try
    {
      UIManager.setLookAndFeel(new MetalLookAndFeel());
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }    
    MyMetalScrollBarUI ui = new MyMetalScrollBarUI();
    harness.check(ui.getScrollBarWidthField(), 0);
    ui.setScrollbar(new JScrollBar());
    ui.installDefaults();
    
    // the scrollBarWidth field must be initialised, the JGoodies Plastic
    // look and feel relies on this...
    harness.check(ui.getScrollBarWidthField(), 17);    
  }
}
