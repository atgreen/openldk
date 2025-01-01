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
 * Tests the isEmpty() method of the {@link Arc2D} class.
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
    Arc2D arc = new Arc2D.Double();
    harness.check(!arc.contains(0.0, 0.0));  // check 1
    
    arc = new Arc2D.Double(-1.0, -1.0, 2.0, 2.0, 0.0, 90.0, Arc2D.CHORD);
    harness.check(!arc.contains(0.0, 0.0));  // check 2
    harness.check(!arc.contains(1.0, 0.0));  // check 3
    harness.check(!arc.contains(0.0, 1.0));  // check 4
    harness.check(!arc.contains(0.5, 0.5));  // check 5
    harness.check(!arc.contains(0.5, -0.5)); // check 6

    arc = new Arc2D.Double(-1.0, -1.0, 2.0, 2.0, 0.0, 90.0, Arc2D.PIE);
    harness.check(arc.contains(0.0, 0.0));   // check 7
    harness.check(!arc.contains(1.0, 0.0));  // check 8
    harness.check(!arc.contains(0.0, 1.0));  // check 9
    harness.check(!arc.contains(0.5, 0.5));  // check 10
    harness.check(arc.contains(0.5, -0.5));  // check 11
    
    arc = new Arc2D.Double(-1.0, -1.0, 2.0, 2.0, 0.0, 90.0, Arc2D.OPEN);
    harness.check(!arc.contains(0.0, 0.0));  // check 12
    harness.check(!arc.contains(1.0, 0.0));  // check 13
    harness.check(!arc.contains(0.0, 1.0));  // check 14
    harness.check(!arc.contains(0.5, 0.5));  // check 15
    harness.check(!arc.contains(0.5, -0.5)); // check 16
    
    // contains(double, double, double, double)
    arc = new Arc2D.Double();
    harness.check(!arc.contains(0.0, 0.0, 0.0, 0.0));  // check 17
    
    arc = new Arc2D.Double(-1.0, -1.0, 2.0, 2.0, 0.0, 90.0, Arc2D.CHORD);
    harness.check(!arc.contains(0.45, -0.55, 0.1, 0.1));  // check 18
   
    arc = new Arc2D.Double(-1.0, -1.0, 2.0, 2.0, 0.0, 90.0, Arc2D.PIE);
    harness.check(arc.contains(0.45, -0.55, 0.1, 0.1));   // check 19
   
    arc = new Arc2D.Double(-1.0, -1.0, 2.0, 2.0, 0.0, 90.0, Arc2D.OPEN);
    harness.check(!arc.contains(0.45, -0.55, 0.1, 0.1));  // check 20
    
    // contains(Rectangle2D)
    arc = new Arc2D.Double();
    harness.check(                                       // check 21
      !arc.contains(new Rectangle2D.Double(0.0, 0.0, 0.0, 0.0))
    );
    
    Rectangle2D r = new Rectangle2D.Double(0.45, -0.55, 0.1, 0.1);
    arc = new Arc2D.Double(-1.0, -1.0, 2.0, 2.0, 0.0, 90.0, Arc2D.CHORD);
    harness.check(!arc.contains(r));                     // check 22
   
    arc = new Arc2D.Double(-1.0, -1.0, 2.0, 2.0, 0.0, 90.0, Arc2D.PIE);
    harness.check(arc.contains(r));                      // check 23
   
    arc = new Arc2D.Double(-1.0, -1.0, 2.0, 2.0, 0.0, 90.0, Arc2D.OPEN);
    harness.check(!arc.contains(r));                     // check 24
     
    boolean pass = false;
    try
    {
      arc.contains((Rectangle2D) null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);  // check 25
  }

}
