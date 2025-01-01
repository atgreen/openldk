// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.GradientPaint;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.geom.Point2D;

/**
 * Some checks for the constructors in the {@link GradientPaint} class.  
 */
public class constructors implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
  }

  private void testConstructor1(TestHarness harness) 
  {
    harness.checkPoint("(float, float, Color, float, float, Color)");
    GradientPaint gp = new GradientPaint(1.0f, 2.0f, Color.red, 3.0f, 4.0f, Color.blue);
    harness.check(gp.getPoint1().getX(), 1.0);
    harness.check(gp.getPoint1().getY(), 2.0);
    harness.check(gp.getColor1(), Color.red);
    harness.check(gp.getPoint2().getX(), 3.0);
    harness.check(gp.getPoint2().getY(), 4.0);
    harness.check(gp.getColor2(), Color.blue);
    harness.check(gp.isCyclic(), false);
    
    // try null arguments
    boolean pass = false;
    try
    {
      gp = new GradientPaint(1.0f, 2.0f, null, 3.0f, 4.0f, Color.blue);   
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      gp = new GradientPaint(1.0f, 2.0f, Color.red, 3.0f, 4.0f, null);   
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);  
  }

  private void testConstructor2(TestHarness harness) 
  {
    harness.checkPoint("(float, float, Color, float, float, Color, boolean)");
    GradientPaint gp = new GradientPaint(1.0f, 2.0f, Color.red, 3.0f, 4.0f, Color.blue, true);
    harness.check(gp.getPoint1().getX(), 1.0);
    harness.check(gp.getPoint1().getY(), 2.0);
    harness.check(gp.getColor1(), Color.red);
    harness.check(gp.getPoint2().getX(), 3.0);
    harness.check(gp.getPoint2().getY(), 4.0);
    harness.check(gp.getColor2(), Color.blue);
    harness.check(gp.isCyclic(), true);
    
    // try null arguments
    boolean pass = false;
    try
    {
      gp = new GradientPaint(1.0f, 2.0f, null, 3.0f, 4.0f, Color.blue, true);   
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      gp = new GradientPaint(1.0f, 2.0f, Color.red, 3.0f, 4.0f, null, true);   
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);  
  }

  private void testConstructor3(TestHarness harness) 
  {
    harness.checkPoint("(Point2D, Color, Point2D, Color)");
    GradientPaint gp = new GradientPaint(
      new Point2D.Float(1.0f, 2.0f), Color.red, 
      new Point2D.Float(3.0f, 4.0f), Color.blue
    );
    harness.check(gp.getPoint1().getX(), 1.0);
    harness.check(gp.getPoint1().getY(), 2.0);
    harness.check(gp.getColor1(), Color.red);
    harness.check(gp.getPoint2().getX(), 3.0);
    harness.check(gp.getPoint2().getY(), 4.0);
    harness.check(gp.getColor2(), Color.blue);
    harness.check(gp.isCyclic(), false);
    
    // try null arguments
    boolean pass = false;
    try
    {
      gp = new GradientPaint(null, Color.red, new Point2D.Float(3.0f, 4.0f), Color.blue);   
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      gp = new GradientPaint(new Point2D.Float(1.0f, 2.0f), null, new Point2D.Float(3.0f, 4.0f), Color.blue);   
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);  

    pass = false;
    try
    {
      gp = new GradientPaint(new Point2D.Float(1.0f, 2.0f), Color.red, null, Color.blue);   
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);  
  
    pass = false;
    try
    {
      gp = new GradientPaint(new Point2D.Float(1.0f, 2.0f), Color.red, new Point2D.Float(1.0f, 2.0f), null);   
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);  
  }

  private void testConstructor4(TestHarness harness) 
  {
    harness.checkPoint("(Point2D, Color, Point2D, Color, boolean)");
    GradientPaint gp = new GradientPaint(
      new Point2D.Float(1.0f, 2.0f), Color.red, 
      new Point2D.Float(3.0f, 4.0f), Color.blue, true
    );
    harness.check(gp.getPoint1().getX(), 1.0);
    harness.check(gp.getPoint1().getY(), 2.0);
    harness.check(gp.getColor1(), Color.red);
    harness.check(gp.getPoint2().getX(), 3.0);
    harness.check(gp.getPoint2().getY(), 4.0);
    harness.check(gp.getColor2(), Color.blue);
    harness.check(gp.isCyclic(), true);
    
    // try null arguments
    boolean pass = false;
    try
    {
      gp = new GradientPaint(null, Color.red, new Point2D.Float(3.0f, 4.0f), Color.blue, true);   
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      gp = new GradientPaint(new Point2D.Float(1.0f, 2.0f), null, new Point2D.Float(3.0f, 4.0f), Color.blue, true);   
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);  

    pass = false;
    try
    {
      gp = new GradientPaint(new Point2D.Float(1.0f, 2.0f), Color.red, null, Color.blue, true);   
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);  
  
    pass = false;
    try
    {
      gp = new GradientPaint(new Point2D.Float(1.0f, 2.0f), Color.red, new Point2D.Float(1.0f, 2.0f), null, true);   
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);  
  }

}
