// Tags: JDK1.4

// Copyright (C) 2004 David Gilbert (david.gilbert@object-refinery.com)

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.AWTKeyStroke;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.AWTKeyStroke;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

/**
 * Some checks for the equals() method.
 */
public class equals
  implements Testlet
{
  /**
   * Some checks for the equals() method.
   */
  public void test(TestHarness harness)
  {
    AWTKeyStroke ks1 = AWTKeyStroke.getAWTKeyStroke('A');
    AWTKeyStroke ks2 = AWTKeyStroke.getAWTKeyStroke('A');
    harness.check(ks1.equals(ks2));
    
    harness.check(!ks1.equals(null));
    harness.check(!ks1.equals(new Integer(42)));
    
    ks1 = AWTKeyStroke.getAWTKeyStroke('a');
    harness.check(!ks1.equals(ks2));
    ks2 = AWTKeyStroke.getAWTKeyStroke('a');
    harness.check(ks1.equals(ks2));

    ks1 = AWTKeyStroke.getAWTKeyStroke(new Character('a'), InputEvent.SHIFT_DOWN_MASK);
    harness.check(!ks1.equals(ks2));
    ks2 = AWTKeyStroke.getAWTKeyStroke(new Character('a'), InputEvent.SHIFT_DOWN_MASK);
    harness.check(ks1.equals(ks2));

    ks1 = AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_A, InputEvent.SHIFT_DOWN_MASK, false);
    harness.check(!ks1.equals(ks2));
    ks2 = AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_A, InputEvent.SHIFT_DOWN_MASK, false);
    harness.check(ks1.equals(ks2));

    ks1 = AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_A, InputEvent.SHIFT_DOWN_MASK, true);
    harness.check(!ks1.equals(ks2));
    ks2 = AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_A, InputEvent.SHIFT_DOWN_MASK, true);
    harness.check(ks1.equals(ks2));
  }

}