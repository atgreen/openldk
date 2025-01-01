// Tags: JDK1.4

// Copyright (C) 2012 Pavel Tisnovsky <ptisnovs@redhat.com>

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
// the Free Software Foundation, Inc., 51 Franklin Street,
// Fifth Floor, Boston, MA 02110-1301 USA.

package gnu.testlet.java.awt.BasicStroke;

import java.awt.Shape;
import java.awt.BasicStroke;
import java.awt.geom.Line2D;
import java.awt.geom.Line2D.Double;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/* Test of method BasicStroke.createShape() called for
 * a shape created from Line2D.Double */
public class createStrokeShapeLine2DDouble implements Testlet
{
  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    BasicStroke basicStroke = new BasicStroke(10.0f);
    Shape s1 = new Line2D.Double(0, 0, 100, 100);
    Shape s2 = basicStroke.createStrokedShape(s1);

    /* basic tests - line vertexes */
    harness.check(s2.contains(  0,  0));
    harness.check(s2.contains( 50, 50));
    harness.check(s2.contains(100,100));

    /* basic negative test */
    harness.check(!s2.contains(  0,100));
    harness.check(!s2.contains(100,  0));

    /* positive tests */
    for (int i = 0; i < 100; i++)
    {
      harness.check(s2.contains(i, i));
    }

    /* negative tests */
    harness.check(!s2.contains(50, 40));
    harness.check(!s2.contains(50, 60));
    harness.check(!s2.contains(40, 50));
    harness.check(!s2.contains(60, 50));
  }
}

