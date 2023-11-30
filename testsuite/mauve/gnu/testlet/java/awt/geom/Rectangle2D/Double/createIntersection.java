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

package gnu.testlet.java.awt.geom.Rectangle2D.Double;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.geom.Rectangle2D;

/**
 * Some checks for the createIntersection() method in the 
 * {@link Rectangle2D.Double} class.
 */
public class createIntersection implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    // empty rectangles
    Rectangle2D r0 = new Rectangle2D.Double();
    Rectangle2D r1 = r0.createIntersection(new Rectangle2D.Double());
    harness.check(r1.getX(), 0.0);
    harness.check(r1.getY(), 0.0);
    harness.check(r1.getWidth(), 0.0);
    harness.check(r1.getHeight(), 0.0);
    
    // regular intersection
    r0 = new Rectangle2D.Double(1.0, 2.0, 3.0, 4.0);
    r1 = new Rectangle2D.Double(1.5, 2.5, 3.5, 4.5);
    Rectangle2D r2 = r0.createIntersection(r1);
    harness.check(r2.getX(), 1.5);
    harness.check(r2.getY(), 2.5);
    harness.check(r2.getWidth(), 2.5);
    harness.check(r2.getHeight(), 3.5);
    
    // no intersection
    r0 = new Rectangle2D.Double(1.0, 1.0, 1.0, 1.0);
    r1 = new Rectangle2D.Double(3.0, 3.0, 1.0, 1.0);
    r2 = r0.createIntersection(r1);
    
    // strictly speaking, the spec only says that 'r2' should be
    // empty...
    harness.check(r2.isEmpty());
    
    // ...but let's check the actual numbers anyway for the sake 
    // of compatibility.
    harness.check(r2.getX(), 3.0);
    harness.check(r2.getY(), 3.0);
    harness.check(r2.getWidth(), -1.0);
    harness.check(r2.getHeight(), -1.0); 
  }

}
