// Tags: JDK1.1

// Copyright (C) 2004, 2006, David Gilbert <david.gilbert@object-refinery.com>

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version. 

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to the
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301 USA.

package gnu.testlet.java.awt.Point;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Point;

/**
 * Some checks for the setLocation() method in the {@link Point} class.
 */
public class setLocation implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("setLocation(int, int)");
    Point p = new Point();
    p.setLocation(4, 5);
    harness.check(p.x, 4);
    harness.check(p.y, 5);
    
    harness.checkPoint("setLocation(Point)");
    p.setLocation(new Point(6, 7));
    harness.check(p.x, 6);
    harness.check(p.y, 7);

    try
    {
      p.setLocation(null);
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }

    harness.checkPoint("setLocation(double, double)");
    p = new Point();
    p.setLocation(1.2, 2.3);
    harness.check(p.x, 1);
    harness.check(p.y, 2);
    
    p.setLocation(1.51, 2.7);
    harness.check(p.x, 2);
    harness.check(p.y, 3);
    
    p.setLocation(1.5, 2.5);
    harness.check(p.x, 2);
    harness.check(p.y, 3);    

    p.setLocation(-1.5, -2.5);
    harness.check(p.x, -1);
    harness.check(p.y, -2);    

    p.setLocation(1.499, 2.499);
    harness.check(p.x, 1);
    harness.check(p.y, 2);  
    
    p.setLocation(Double.NaN, Double.NaN);
    harness.check(p.x, 0);
    harness.check(p.y, 0);      

    double bigPos = Integer.MAX_VALUE + 10000.0;
    double bigNeg = Integer.MIN_VALUE - 10000.0;
    
    p.setLocation(bigPos, bigPos);
    harness.check(p.x, Integer.MAX_VALUE);
    harness.check(p.y, Integer.MAX_VALUE);  
    
    p.setLocation(bigNeg, bigNeg);    
    harness.check(p.x, Integer.MIN_VALUE);
    harness.check(p.y, Integer.MIN_VALUE);
  }

}
