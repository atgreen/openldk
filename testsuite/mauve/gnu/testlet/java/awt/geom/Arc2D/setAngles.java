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

package gnu.testlet.java.awt.geom.Arc2D;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.geom.Arc2D;
import java.awt.geom.Point2D;

/**
 * Tests the setAngles() method of the {@link Arc2D} class.
 */
public class setAngles implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)       
  {
    // setAngles(double, double, double, double)
    Arc2D arc = new Arc2D.Double(-1.0, -1.0, 2.0, 2.0, 0.0, 90.0, Arc2D.PIE);
    arc.setAngles(1.0, -1.0, -1.0, -1.0);
    harness.check(arc.getAngleStart(), 45.0);
    harness.check(arc.getAngleExtent(), 90.0);
   
    // setAngleStart(Point2D, Point2D)
    arc.setAngles(new Point2D.Double(1.0, 1.0), new Point2D.Double(-1.0, 1.0));
    harness.check(arc.getAngleStart(), -45.0);
    harness.check(arc.getAngleExtent(), 270.0);
    
    boolean pass = false;
    try
    {
      arc.setAngles(null, new Point2D.Double(-1.0, 1.0));
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      arc.setAngles(new Point2D.Double(-1.0, 1.0), null);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

}
