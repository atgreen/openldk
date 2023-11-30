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
import java.awt.geom.Rectangle2D;

/**
 * Tests the constructors for the {@link Arc2D} class.
 */
public class constructors implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    // constructor 1
    Arc2D arc = new Arc2D.Double();
    harness.check(arc.getArcType() == Arc2D.OPEN);
    harness.check(arc.getX(), 0.0);
    harness.check(arc.getY(), 0.0);
    harness.check(arc.getWidth(), 0.0);
    harness.check(arc.getHeight(), 0.0);
    harness.check(arc.getAngleStart(), 0.0);
    harness.check(arc.getAngleExtent(), 0.0);
    
    // constructor 2
    arc = new Arc2D.Double(Arc2D.PIE);
    harness.check(arc.getArcType() == Arc2D.PIE);
    harness.check(arc.getX(), 0.0);
    harness.check(arc.getY(), 0.0);
    harness.check(arc.getWidth(), 0.0);
    harness.check(arc.getHeight(), 0.0);
    harness.check(arc.getAngleStart(), 0.0);
    harness.check(arc.getAngleExtent(), 0.0);
    
    // constructor 3
    arc = new Arc2D.Double(1.0, 2.0, 3.0, 4.0, 45, 90.0, Arc2D.CHORD);
    harness.check(arc.getArcType() == Arc2D.CHORD);
    harness.check(arc.getX(), 1.0);
    harness.check(arc.getY(), 2.0);
    harness.check(arc.getWidth(), 3.0);
    harness.check(arc.getHeight(), 4.0);
    harness.check(arc.getAngleStart(), 45.0);
    harness.check(arc.getAngleExtent(), 90.0);
    
    boolean pass = false;
    try 
    {
      arc = new Arc2D.Double(1.0, 2.0, 3.0, 4.0, 45, 90.0, 99); 
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // constructor 4
    arc = new Arc2D.Double(new Rectangle2D.Double(10.0, 11.0, 12.0, 13.0), 
            110.0, 35.0, Arc2D.OPEN);
    harness.check(arc.getArcType() == Arc2D.OPEN);
    harness.check(arc.getX(), 10.0);
    harness.check(arc.getY(), 11.0);
    harness.check(arc.getWidth(), 12.0);
    harness.check(arc.getHeight(), 13.0);
    harness.check(arc.getAngleStart(), 110.0);
    harness.check(arc.getAngleExtent(), 35.0);

    pass = false;
    try 
    {
      arc = new Arc2D.Double(null, 110.0, 35.0, Arc2D.OPEN);        
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

}
