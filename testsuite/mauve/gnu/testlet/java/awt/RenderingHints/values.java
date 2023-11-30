//Tags: JDK1.2

//Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.RenderingHints;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Collection;

import java.awt.RenderingHints;

/**
 * Some checks for the values() method in the {@link RenderingHints} class.
 */
public class values implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) {

    RenderingHints h1 = new RenderingHints(null);
    Collection v1 = h1.values();
    harness.check(v1.isEmpty());

    h1.put(RenderingHints.KEY_DITHERING, RenderingHints.VALUE_DITHER_DISABLE);
    // the values collection is backed by the RenderingHints, so it updates
    harness.check(v1.size() == 1);
    harness.check(v1.contains(RenderingHints.VALUE_DITHER_DISABLE));
    
  }

}
