// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.java.awt.AlphaComposite;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.AlphaComposite;

/**
 * Some checks for the equals() method in the {@link AlphaComposite} class
 * using constants available since 1.4.  
 */
public class equals implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    AlphaComposite a1 = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3f);
    AlphaComposite a2 = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.4f);
    harness.check(!a1.equals(a2));
    harness.check(!a1.equals(null));
    a2 = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3f);
    harness.check(a1.equals(a2));
  }

}
