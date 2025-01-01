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

package gnu.testlet.java.awt.geom.Area;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;

/**
 * Some tests for the isRectangular() method in the {@link Area} class.
 */
public class isRectangular implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {

    // an empty area is treated as being rectangular
    Area area = new Area();
    harness.check(area.isRectangular());
    area = new Area(new Ellipse2D.Double());
    harness.check(area.isRectangular());

    area.add(new Area(new Rectangle2D.Double(1.0, 1.0, 1.0, 1.0)));
    harness.check(area.isRectangular());
    area.add(new Area(new Rectangle2D.Double(10.0, 10.0, 1.0, 1.0)));
    harness.check(!area.isRectangular());
    
    area = new Area(new Ellipse2D.Double(1.0, 2.0, 3.0, 4.0));
    harness.check(!area.isRectangular());
  }

}
