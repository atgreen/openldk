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
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

/**
 * Tests the contains() method for the {@link Area} class.
 */
public class contains implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    // contains(double, double)
    Area area = new Area();
    harness.check(!area.contains(0.0, 0.0));
    
    area.add(new Area(new Rectangle2D.Double(1.0, 1.0, 1.0, 1.0)));
    harness.check(area.contains(1.0, 1.0));
    harness.check(!area.contains(1.0, 2.0));
    harness.check(!area.contains(2.0, 1.0));
    harness.check(!area.contains(2.0, 2.0));

    // contains(Point2D)
    boolean pass = false;
    try
    {
      area.contains((Point2D) null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // contains(Rectangle2D)
    pass = false;
    try
    {
      area.contains((Rectangle2D) null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
  }

}
