// Tags: JDK1.2

// Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.java.awt.geom.Ellipse2D.Float;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.geom.Ellipse2D;

/**
 * Some checks for the getWidth() method of the {@link Ellipse2D.Float} class.
 */
public class getWidth implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)       
  {
    Ellipse2D e = new Ellipse2D.Float();
    harness.check(e.getWidth(), 0.0);

    e = new Ellipse2D.Float(1.0f, 2.0f, 3.0f, 4.0f);
    harness.check(e.getWidth(), 3.0);
  }

}
