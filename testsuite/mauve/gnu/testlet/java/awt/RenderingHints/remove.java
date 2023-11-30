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

import java.awt.RenderingHints;

/**
 * Some checks for the remove() method in the {@link RenderingHints} class.
 */
public class remove implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) {

    RenderingHints h1 = new RenderingHints(null);
    Object result = h1.remove(RenderingHints.KEY_ALPHA_INTERPOLATION);
    harness.check(result == null);
    h1.put(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
    result = h1.remove(RenderingHints.KEY_RENDERING);
    harness.check(result == RenderingHints.VALUE_RENDER_QUALITY);
    harness.check(h1.isEmpty());
   
    result = h1.remove(null);
    harness.check(result == null);
    
    boolean pass = false;
    try
    {
      h1.remove(new Integer(1));
    }
    catch (ClassCastException e)
    {
      pass = true;
    }
    harness.check(pass);

  }

}
