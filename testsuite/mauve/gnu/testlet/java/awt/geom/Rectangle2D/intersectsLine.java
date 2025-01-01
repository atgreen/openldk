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

package gnu.testlet.java.awt.geom.Rectangle2D;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.geom.Rectangle2D;

/**
 * Checks that the intersectsLine() method works correctly.
 */
public class intersectsLine implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    Rectangle2D r = new Rectangle2D.Double(0.0, 0.0, 5.0, 10.0);
    harness.check(r.intersectsLine(0.0, 0.0, 5.0, 0.0));
    harness.check(r.intersectsLine(0.0, 0.0, 0.0, 10.0));
    harness.check(r.intersectsLine(5.0, 0.0, 5.0, 10.0));
    harness.check(r.intersectsLine(0.0, 10.0, 5.0, 10.0));
  
    // segment that doesn't intersect
    harness.check(!r.intersectsLine(-1.0, -1.0, -2.0, -2.0));
    
    // segment passes through
    harness.check(r.intersectsLine(-5.0, 2.0, 15.0, 2.0));
    
    // segments that end at corners
    harness.check(r.intersectsLine(-1.0, 0.0, 0.0, 0.0));
    harness.check(r.intersectsLine(5.0, -1.0, 5.0, 0.0));
    harness.check(r.intersectsLine(-1.0, 10.0, 0.0, 10.0));
    harness.check(r.intersectsLine(5, 11.0, 5.0, 10.0));

    // zero rectangle
    r = new Rectangle2D.Double();
    harness.check(!r.intersectsLine(0.0, 0.0, 0.0, 0.0));
    
    boolean pass = false;
    try
    {
      r.intersectsLine(null);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

}
