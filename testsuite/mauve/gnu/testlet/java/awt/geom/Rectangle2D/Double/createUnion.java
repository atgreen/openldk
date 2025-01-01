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
 * Some checks for the createUnion() method in the 
 * {@link Rectangle2D.Double} class.
 */
public class createUnion implements Testlet 
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
    Rectangle2D r1 = r0.createUnion(new Rectangle2D.Double());
    harness.check(r1.getX(), 0.0);
    harness.check(r1.getY(), 0.0);
    harness.check(r1.getWidth(), 0.0);
    harness.check(r1.getHeight(), 0.0);
  
    // overlapping
    r0 = new Rectangle2D.Double(1.0, 2.0, 3.0, 4.0);
    r1 = new Rectangle2D.Double(1.5, 2.5, 3.5, 4.5);
    Rectangle2D r2 = r0.createUnion(r1);
    harness.check(r2.getX(), 1.0);
    harness.check(r2.getY(), 2.0);
    harness.check(r2.getWidth(), 4.0);
    harness.check(r2.getHeight(), 5.0);
  
    // non-overlapping
    r0 = new Rectangle2D.Double(1.0, 1.0, 1.0, 1.0);
    r1 = new Rectangle2D.Double(3.0, 3.0, 1.0, 1.0);
    r2 = r0.createUnion(r1);
    harness.check(r2.getX(), 1.0);
    harness.check(r2.getY(), 1.0);
    harness.check(r2.getWidth(), 3.0);
    harness.check(r2.getHeight(), 3.0); 
  }

}
