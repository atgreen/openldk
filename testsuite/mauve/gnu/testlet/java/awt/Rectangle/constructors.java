/* constructors.java 
   Copyright (C) 2006 RedHat
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: 1.4

package gnu.testlet.java.awt.Rectangle;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class constructors implements Testlet
{

  public void test(TestHarness harness)
  {
    testConstructor1(harness); 
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
    testConstructor5(harness);
    testConstructor6(harness);
    testConstructor7(harness);
  }
  
  public void testConstructor1(TestHarness harness)
  {
    Rectangle r = new Rectangle();
    harness.check(r.getX(), 0);
    harness.check(r.getY(), 0);
    harness.check(r.getWidth(), 0);
    harness.check(r.getHeight(), 0);
  }

  public void testConstructor2(TestHarness harness)
  {
    Rectangle r1 = new Rectangle();
    Rectangle r2 = new Rectangle(r1);
    harness.check(r2.getX(), 0);
    harness.check(r2.getY(), 0);
    harness.check(r2.getWidth(), 0);
    harness.check(r2.getHeight(), 0);
    
    Rectangle r3 = new Rectangle(1, 2, 3, 4);
    Rectangle r4 = new Rectangle(r3);
    harness.check(r4.getX(), 1);
    harness.check(r4.getY(), 2);
    harness.check(r4.getWidth(), 3);
    harness.check(r4.getHeight(), 4);
    
    boolean failed = false;
    Rectangle r5 = null;
    try
      {
        Rectangle r6 = new Rectangle(r5);
      }
    catch (NullPointerException e)
      {
        failed = true;
      }
    harness.check(failed);
  }
  
  public void testConstructor3(TestHarness harness)
  {
    Rectangle r1 = new Rectangle(5, 10, 15, 20);
    harness.check(r1.getX(), 5);
    harness.check(r1.getY(), 10);
    harness.check(r1.getWidth(), 15);
    harness.check(r1.getHeight(), 20);
    
    Rectangle r2 = new Rectangle(-5, -10, -15, -20);
    harness.check(r2.getX(), -5);
    harness.check(r2.getY(),-10);
    harness.check(r2.getWidth(), -15);
    harness.check(r2.getHeight(), -20);
  }
  
  public void testConstructor4(TestHarness harness)
  {
    Rectangle r1 = new Rectangle(5, 10);
    harness.check(r1.getX(), 0);
    harness.check(r1.getY(), 0);
    harness.check(r1.getWidth(), 5);
    harness.check(r1.getHeight(), 10);
    
    Rectangle r2 = new Rectangle(-5, -10);
    harness.check(r2.getX(), 0);
    harness.check(r2.getY(), 0);
    harness.check(r2.getWidth(), -5);
    harness.check(r2.getHeight(), -10);
  }
  
  public void testConstructor5(TestHarness harness)
  {
    Point p1 = new Point();
    Dimension d1 = new Dimension();
    Rectangle r1 = new Rectangle(p1, d1);
    harness.check(r1.getX(), 0);
    harness.check(r1.getY(), 0);
    harness.check(r1.getWidth(), 0);
    harness.check(r1.getHeight(), 0);
    
    Point p2 = new Point(5, 10);
    Dimension d2 = new Dimension(15, 20);
    Rectangle r2 = new Rectangle(p2, d2);
    harness.check(r2.getX(), 5);
    harness.check(r2.getY(), 10);
    harness.check(r2.getWidth(), 15);
    harness.check(r2.getHeight(), 20);
    
    boolean failed1 = false;
    Point p3 = null;
    Dimension d3 = new Dimension();
    try
      {
        Rectangle r3 = new Rectangle(p3, d3);
      }
    catch (NullPointerException e)
      {
        failed1 = true;
      }
    harness.check(failed1);
    
    boolean failed2 = false;
    Point p4 = new Point();
    Dimension d4 = null;
    try
      {
        Rectangle r4 = new Rectangle(p3, d3);
      }
    catch (NullPointerException e)
      {
        failed2 = true;
      }
    harness.check(failed2);
    
    Point p5 = new Point(-5, -10);
    Dimension d5 = new Dimension(-15, -20);
    Rectangle r5 = new Rectangle(p5, d5);
    harness.check(r2.getX(), 5);
    harness.check(r2.getY(), 10);
    harness.check(r2.getWidth(), 15);
    harness.check(r2.getHeight(), 20);
  }
  
  public void testConstructor6(TestHarness harness)
  {
    Point p1 = new Point();
    Rectangle r1 = new Rectangle(p1);
    harness.check(r1.getX(), 0);
    harness.check(r1.getY(), 0);
    harness.check(r1.getWidth(), 0);
    harness.check(r1.getHeight(), 0);
    
    Point p2 = new Point(5, 10);
    Rectangle r2 = new Rectangle(p2);
    harness.check(r2.getX(), 5);
    harness.check(r2.getY(), 10);
    harness.check(r2.getWidth(), 0);
    harness.check(r2.getHeight(), 0);
    
    boolean failed = false;
    Point p3 = null;
    try
      {
        Rectangle r3 = new Rectangle(p3);
      }
    catch (NullPointerException e)
      {
        failed = true;
      }
    harness.check(failed);
    
    Point p4 = new Point(-5, -10);
    Rectangle r4 = new Rectangle(p4);
    harness.check(r4.getX(), -5);
    harness.check(r4.getY(), -10);
    harness.check(r4.getWidth(), 0);
    harness.check(r4.getHeight(), 0);
  }
  
  public void testConstructor7(TestHarness harness)
  {
    Dimension d1 = new Dimension();
    Rectangle r1 = new Rectangle(d1);
    harness.check(r1.getX(), 0);
    harness.check(r1.getY(), 0);
    harness.check(r1.getWidth(), 0);
    harness.check(r1.getHeight(), 0);
    
    Dimension d2 = new Dimension(15, 20);
    Rectangle r2 = new Rectangle(d2);
    harness.check(r2.getX(), 0);
    harness.check(r2.getY(), 0);
    harness.check(r2.getWidth(), 15);
    harness.check(r2.getHeight(), 20);
    
    boolean failed = false;
    Dimension d3 = null;
    try
      {
        Rectangle r3 = new Rectangle(d3);
      }
    catch (NullPointerException e)
      {
        failed = true;
      }
    harness.check(failed);
    
    Dimension d4 = new Dimension(-15, -20);
    Rectangle r4 = new Rectangle(d4);
    harness.check(r4.getX(), 0);
    harness.check(r4.getY(), 0);
    harness.check(r4.getWidth(), -15);
    harness.check(r4.getHeight(), -20);
  }
  
}
