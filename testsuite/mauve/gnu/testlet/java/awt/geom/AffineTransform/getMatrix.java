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
 * Some tests for the {@link AffineTransform} class.
 */
public class getMatrix implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    AffineTransform t = new AffineTransform(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
    double[] m = new double[4];
    t.getMatrix(m);
    harness.check(m[0], 1.0);
    harness.check(m[1], 2.0);
    harness.check(m[2], 3.0);
    harness.check(m[3], 4.0);
    
    m = new double[6];
    t.getMatrix(m);
    harness.check(m[0], 1.0);
    harness.check(m[1], 2.0);
    harness.check(m[2], 3.0);
    harness.check(m[3], 4.0);
    harness.check(m[4], 5.0);
    harness.check(m[5], 6.0);
    
    boolean pass = false;
    m = new double[2];
    try
    {
      t.getMatrix(m);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      t.getMatrix(null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

}
