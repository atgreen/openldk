//Tags: JDK1.4

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

package gnu.testlet.java.awt.geom.AffineTransform;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.geom.AffineTransform;

/**
 * Verifies constant values for the {@link AffineTransform} class.
 */
public class constants implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.check(AffineTransform.TYPE_FLIP == 64);
    harness.check(AffineTransform.TYPE_GENERAL_ROTATION == 16);
    harness.check(AffineTransform.TYPE_GENERAL_SCALE == 4);
    harness.check(AffineTransform.TYPE_GENERAL_TRANSFORM == 32);
    harness.check(AffineTransform.TYPE_IDENTITY == 0);
    harness.check(AffineTransform.TYPE_MASK_ROTATION == 24);
    harness.check(AffineTransform.TYPE_MASK_SCALE == 6);
    harness.check(AffineTransform.TYPE_QUADRANT_ROTATION == 8);
    harness.check(AffineTransform.TYPE_TRANSLATION == 1);
    harness.check(AffineTransform.TYPE_UNIFORM_SCALE == 2);
  }

}
