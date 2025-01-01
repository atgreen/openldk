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
import java.awt.geom.Point2D;

/**
 * Some tests for the {@link AffineTransform} class.
 */
public class transform implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    AffineTransform t1 = AffineTransform.getScaleInstance(2.0, 3.0);
    double[] d = new double[] { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };
    double[] expected = new double[] { 1.0, 2.0, 6.0, 12.0, 10.0, 18.0, 14.0, 24.0, 9.0, 10.0 };
    t1.transform(d, 2, d, 2, 3);
    for (int i = 0; i < 10; i++) {
      harness.check(d[i], expected[i]);
    }
  
    d = new double[] { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };
    float[] f = new float[] { 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f, 9.0f, 10.0f };
    float[] expectedf = new float[] { 1.0f, 2.0f, 6.0f, 12.0f, 10.0f, 18.0f, 14.0f, 24.0f, 9.0f, 10.0f };
    t1.transform(d, 2, f, 2, 3);
    for (int i = 0; i < 10; i++) {
      harness.check(f[i], expectedf[i]);
    }
    
    f = new float[] { 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f, 9.0f, 10.0f };
    d = new double[] { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };
    t1.transform(f, 2, d, 2, 3);
    for (int i = 0; i < 10; i++) {
      harness.check(d[i], expected[i]);
    }

    t1.transform(f, 2, f, 2, 3);
    for (int i = 0; i < 10; i++) {
      harness.check(f[i], expectedf[i]);
    }
    
    Point2D p1 = new Point2D.Double(1.0, 2.0);
    Point2D p2 = t1.transform(p1, null);
    harness.check(p2.getX(), 2.0);
    harness.check(p2.getY(), 6.0);
  
    t1.transform(p1, p1);
    harness.check(p1.getX(), 2.0);
    harness.check(p1.getY(), 6.0);
  
    boolean pass = false;
    try
    {
      t1.transform(null, null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  } 

}
