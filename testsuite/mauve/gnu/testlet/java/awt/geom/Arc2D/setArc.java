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

import java.awt.Dimension;
import java.awt.geom.Arc2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

/**
 * Tests the setArc() method of the {@link Arc2D} class.
 */
public class setArc implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)       
  {
    // setArc(Arc2D)
    Arc2D arc1 = new Arc2D.Double();
    Arc2D arc2 = new Arc2D.Double(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, Arc2D.PIE);
    arc1.setArc(arc2);
    harness.check(arc1.getX(), 1.0);
    harness.check(arc1.getY(), 2.0);
    harness.check(arc1.getWidth(), 3.0);
    harness.check(arc1.getHeight(), 4.0);
    harness.check(arc1.getAngleStart(), 5.0);
    harness.check(arc1.getAngleExtent(), 6.0);
    harness.check(arc1.getArcType() == Arc2D.PIE);
    
    boolean pass = false;
    try 
    {
      arc1.setArc(null);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // setArc(double, double, double, double, double, double, int)
    arc1 = new Arc2D.Double();
    arc1.setArc(6.0, 5.0, 4.0, 3.0, 2.0, 1.0, Arc2D.OPEN);
    harness.check(arc1.getX(), 6.0);
    harness.check(arc1.getY(), 5.0);
    harness.check(arc1.getWidth(), 4.0);
    harness.check(arc1.getHeight(), 3.0);
    harness.check(arc1.getAngleStart(), 2.0);
    harness.check(arc1.getAngleExtent(), 1.0);
    harness.check(arc1.getArcType() == Arc2D.OPEN);
    
    // setArc(Point2D, Dimension2D, double, double, int)
    arc1 = new Arc2D.Double();
    arc1.setArc(
      new Point2D.Double(1.0, 2.0), 
      new Dimension(3, 4), 
      5.0, 6.0, Arc2D.CHORD
    );
    harness.check(arc1.getX(), 1.0);
    harness.check(arc1.getY(), 2.0);
    harness.check(arc1.getWidth(), 3.0);
    harness.check(arc1.getHeight(), 4.0);
    harness.check(arc1.getAngleStart(), 5.0);
    harness.check(arc1.getAngleExtent(), 6.0);
    harness.check(arc1.getArcType() == Arc2D.CHORD);
    
    pass = false;
    try 
    {
      arc1.setArc(null, new Dimension(3, 4), 5.0, 6.0, Arc2D.CHORD);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    pass = false;
    try 
    {
      arc1.setArc(new Point2D.Double(1.0, 2.0), null, 5.0, 6.0, Arc2D.CHORD);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // setArc(Rectangle2D, double, double, int)
    arc1 = new Arc2D.Double();
    arc1.setArc(
      new Rectangle2D.Double(1.0, 2.0, 3.0, 4.0),
      5.0, 6.0, Arc2D.OPEN
    );
    harness.check(arc1.getX(), 1.0);
    harness.check(arc1.getY(), 2.0);
    harness.check(arc1.getWidth(), 3.0);
    harness.check(arc1.getHeight(), 4.0);
    harness.check(arc1.getAngleStart(), 5.0);
    harness.check(arc1.getAngleExtent(), 6.0);
    harness.check(arc1.getArcType() == Arc2D.OPEN);
    
    pass = false;
    try 
    {
      arc1.setArc(null, 5.0, 6.0, Arc2D.CHORD);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
  }

}
