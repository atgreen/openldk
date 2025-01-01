//Tags: JDK1.2

//Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.java.awt.geom.RectangularShape;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Rectangle;
import java.awt.geom.Arc2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RectangularShape;
import java.awt.geom.RoundRectangle2D;

/**
 * Some checks for the getFrame() method in the {@link RectangularShape} class.
 */
public class getFrame implements Testlet 
{

  /**
   * Run some tests for an instance of a subclass of {@link RectangularShape}.
   *   
   * @param r  the rectangular shape.
   * @param harness  the test harness.
   */
  public static void testOneInstance(RectangularShape r, TestHarness harness) 
  {
    r.setFrame(1.0, 2.0, 3.0, 4.0);
    Rectangle2D f = r.getFrame();
    harness.check(f.getX(), 1.0);
    harness.check(f.getY(), 2.0);
    harness.check(f.getWidth(), 3.0);
    harness.check(f.getHeight(), 4.0);
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)   
  {
    // it is probably overkill to test all these subclasses, but
    // anyway...
    harness.checkPoint("Arc2D.Float");
    testOneInstance(new Arc2D.Float(), harness);

    harness.checkPoint("Arc2D.Double");
    testOneInstance(new Arc2D.Double(), harness);

    harness.checkPoint("Ellipse2D.Float");
    testOneInstance(new Ellipse2D.Float(), harness);

    harness.checkPoint("Ellipse2D.Double");
    testOneInstance(new Ellipse2D.Double(), harness);

    harness.checkPoint("Rectangle2D.Float");
    testOneInstance(new Rectangle2D.Float(), harness);

    harness.checkPoint("Rectangle2D.Double");
    testOneInstance(new Rectangle2D.Double(), harness);

    harness.checkPoint("RoundRectangle2D.Float");
    testOneInstance(new RoundRectangle2D.Float(), harness);

    harness.checkPoint("RoundRectangle2D.Double");
    testOneInstance(new RoundRectangle2D.Double(), harness);

    harness.checkPoint("Rectangle");
    testOneInstance(new Rectangle(), harness);
  }

}
