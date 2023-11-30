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

package gnu.testlet.java.awt.geom.Ellipse2D.Double;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;

/**
 * Some checks for the setFrame() method of the {@link Ellipse2D.Double} class.
 */
public class setFrame implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)       
  {
    Ellipse2D e = new Ellipse2D.Double(1.0, 2.0, 3.0, 4.0);
    e.setFrame(0.0, 0.0, 0.0, 0.0);
    Rectangle2D b = e.getBounds2D();
    harness.check(b.getX(), 0.0);
    harness.check(b.getY(), 0.0);
    harness.check(b.getWidth(), 0.0);
    harness.check(b.getHeight(), 0.0);

    e = new Ellipse2D.Double();
    e.setFrame(1.0, 2.0, 3.0, 4.0);
    b = e.getBounds2D();
    harness.check(b.getX(), 1.0);
    harness.check(b.getY(), 2.0);
    harness.check(b.getWidth(), 3.0);
    harness.check(b.getHeight(), 4.0);
  }

}
