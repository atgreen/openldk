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
import java.awt.Transparency;

/**
 * Some checks for the getTransparency() method in the {@link GradientPaint} 
 * class works.
 */
public class getTransparency implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    GradientPaint gp = new GradientPaint(1.0f, 2.0f, Color.red, 3.0f, 4.0f, Color.blue);
    harness.check(gp.getTransparency(), Transparency.OPAQUE);
    gp = new GradientPaint(1.0f, 2.0f, new Color(1, 2, 3, 4), 3.0f, 4.0f, Color.blue);
    harness.check(gp.getTransparency(), Transparency.TRANSLUCENT);
    gp = new GradientPaint(1.0f, 2.0f, Color.red, 3.0f, 4.0f, new Color(1, 2, 3, 4));
    harness.check(gp.getTransparency(), Transparency.TRANSLUCENT);
  }

}
