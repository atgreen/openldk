/* setReshowDelay.java -- some checks for the setReshowDelay() method in the
       ToolTipManager class.
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

package gnu.testlet.javax.swing.ToolTipManager;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.ToolTipManager;

public class setReshowDelay implements Testlet {

    public void test(TestHarness harness)
    {
      ToolTipManager m = ToolTipManager.sharedInstance();
      m.setInitialDelay(20);
      m.setReshowDelay(123);
      harness.check(m.getReshowDelay(), 123);
      harness.check(m.getInitialDelay(), 20);
      
      // try zero
      m.setReshowDelay(0);
      harness.check(m.getReshowDelay(), 0);
      
      // try negative
      boolean pass = false;
      try
      {
        m.setReshowDelay(-1);
      }
      catch (IllegalArgumentException e)
      {
        pass = true;
      }
      harness.check(pass);
    }
}
