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

package gnu.testlet.java.awt.geom.AffineTransform;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.geom.AffineTransform;

/**
 * Some tests for the {@link AffineTransform} class.
 */
public class equals implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    AffineTransform t1 = new AffineTransform(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
    AffineTransform t2 = new AffineTransform(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
    harness.check(t1.equals(t2));
    harness.check(t2.equals(t1));
    
    t1 = new AffineTransform(0.0, 2.0, 3.0, 4.0, 5.0, 6.0);
    harness.check(!t1.equals(t2));
    t2 = new AffineTransform(0.0, 2.0, 3.0, 4.0, 5.0, 6.0);
    harness.check(t1.equals(t2));

    t1 = new AffineTransform(1.0, 0.0, 3.0, 4.0, 5.0, 6.0);
    harness.check(!t1.equals(t2));
    t2 = new AffineTransform(1.0, 0.0, 3.0, 4.0, 5.0, 6.0);
    harness.check(t1.equals(t2));

    t1 = new AffineTransform(1.0, 2.0, 0.0, 4.0, 5.0, 6.0);
    harness.check(!t1.equals(t2));
    t2 = new AffineTransform(1.0, 2.0, 0.0, 4.0, 5.0, 6.0);
    harness.check(t1.equals(t2));

    t1 = new AffineTransform(1.0, 2.0, 3.0, 0.0, 5.0, 6.0);
    harness.check(!t1.equals(t2));
    t2 = new AffineTransform(1.0, 2.0, 3.0, 0.0, 5.0, 6.0);
    harness.check(t1.equals(t2));

    t1 = new AffineTransform(1.0, 2.0, 3.0, 4.0, 0.0, 6.0);
    harness.check(!t1.equals(t2));
    t2 = new AffineTransform(1.0, 2.0, 3.0, 4.0, 0.0, 6.0);
    harness.check(t1.equals(t2));

    t1 = new AffineTransform(1.0, 2.0, 3.0, 4.0, 5.0, 0.0);
    harness.check(!t1.equals(t2));
    t2 = new AffineTransform(1.0, 2.0, 3.0, 4.0, 5.0, 0.0);
    harness.check(t1.equals(t2));
  }
}
