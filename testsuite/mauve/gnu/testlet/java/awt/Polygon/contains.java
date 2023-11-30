//Tags: JDK1.0

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

package gnu.testlet.java.awt.Polygon;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Point;
import java.awt.Polygon;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

/**
 * Checks that the contains() method in the {@link Polygon} class works 
 * correctly.
 */
public class contains implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)   
  {
    harness.checkPoint("Polygon 1");
    Polygon p = createTestPolygon1();
    harness.check(p.contains(0, 0));      // 1
    harness.check(!p.contains(0, 1));     // 2
    harness.check(p.contains(1, 0));      // 3
    harness.check(p.contains(1, 1));      // 4
    harness.check(!p.contains(1, 2));     // 5
    harness.check(!p.contains(2, 0));     // 6
    harness.check(!p.contains(2, 1));     // 7
    
    harness.checkPoint("Polygon 2");      
    p = createTestPolygon2();
    harness.check(p.contains(0, 0));      // 8
    harness.check(!p.contains(0, 5));     // 9
    harness.check(!p.contains(5, 5));     // 10
    harness.check(!p.contains(5, 1));     // 11
    harness.check(p.contains(2, 1));      // 12
    harness.check(!p.contains(2, 3));     // 13
    harness.check(!p.contains(3, 3));     // 14
    harness.check(!p.contains(3, 2));     // 15
    harness.check(p.contains(4, 2));      // 16
    harness.check(p.contains(4, 4));      // 17
    harness.check(p.contains(1, 4));      // 18
    harness.check(!p.contains(1, 0));     // 19
    
    harness.check(!p.contains(-0.5, 2.5));  // 20
    harness.check(p.contains(0.5, 2.5));    // 21
    harness.check(!p.contains(1.5, 2.5));   // 22
    harness.check(p.contains(2.5, 2.5));    // 23
    harness.check(!p.contains(3.5, 2.5));   // 24
    harness.check(p.contains(4.5, 2.5));    // 25
    harness.check(!p.contains(5.5, 2.5));   // 26
    
    harness.checkPoint("Null argument checks");
    testNullArguments(harness);
  }
  
  /**
   * Checks method calls with <code>null</code> argument.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  private void testNullArguments(TestHarness harness) 
  {
    boolean pass = false;
    Polygon p = new Polygon();
    try 
    {
      p.contains((Point) null);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    pass = false;
    try 
    {
      p.contains((Point2D) null);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    pass = false;
    try 
    {
      p.contains((Rectangle2D) null);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  /**
   * Creates a simple test polygon.
   * 
   * @return A test polygon.
   */
  private Polygon createTestPolygon1() 
  {
    Polygon p = new Polygon();
    p.addPoint(0, 0);
    p.addPoint(1, 2);
    p.addPoint(2, 0);
    return p;
  }

  /**
   * Creates a more complex test polygon.
   * 
   * @return A test polygon.
   */
  private Polygon createTestPolygon2() 
  {
    Polygon p = new Polygon();
    p.addPoint(0, 0);
    p.addPoint(0, 5);
    p.addPoint(5, 5);
    p.addPoint(5, 1);
    p.addPoint(2, 1);
    p.addPoint(2, 3);
    p.addPoint(3, 3);
    p.addPoint(3, 2);
    p.addPoint(4, 2);
    p.addPoint(4, 4);
    p.addPoint(1, 4);
    p.addPoint(1, 0);
    return p;
  }
  
}
