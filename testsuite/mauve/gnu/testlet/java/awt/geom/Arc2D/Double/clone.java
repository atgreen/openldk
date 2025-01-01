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

package gnu.testlet.java.awt.geom.Arc2D.Double;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.geom.Arc2D;

/**
 * Some checks for the clone() method in the {@link Arc2D.Double} class.
 */
public class clone implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    Arc2D arc1 = new Arc2D.Double(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, Arc2D.CHORD);
    Arc2D arc2 = null;
    arc2 = (Arc2D) arc1.clone();
    harness.check(arc1.getX() == arc2.getX());
    harness.check(arc1.getY() == arc2.getY());
    harness.check(arc1.getWidth() == arc2.getWidth());
    harness.check(arc1.getHeight() == arc2.getHeight());
    harness.check(arc1.getAngleStart(), arc2.getAngleStart());
    harness.check(arc1.getAngleExtent(), arc2.getAngleExtent());
    harness.check(arc1.getArcType() == arc2.getArcType());
    harness.check(arc1.getClass() == arc2.getClass());
    harness.check(arc1 != arc2);
  }

}
