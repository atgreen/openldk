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
    testConstructor5(harness);
    testConstructor6(harness);
  }
  
  private void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("AffineTransform()");
    AffineTransform t = new AffineTransform();
    harness.check(t.getScaleX(), 1.0);
    harness.check(t.getScaleY(), 1.0);
    harness.check(t.getTranslateX(), 0.0);
    harness.check(t.getTranslateY(), 0.0);
    harness.check(t.getShearX(), 0.0);
    harness.check(t.getShearY(), 0.0);
    harness.check(t.getType() == AffineTransform.TYPE_IDENTITY);
  }

  private void testConstructor2(TestHarness harness) 
  {
    harness.checkPoint("AffineTransform(AffineTransform)");
    AffineTransform t1 = new AffineTransform(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
    AffineTransform t2 = new AffineTransform(t1);
    harness.check(t2.getScaleX(), 1.0);
    harness.check(t2.getScaleY(), 4.0);
    harness.check(t2.getTranslateX(), 5.0);
    harness.check(t2.getTranslateY(), 6.0);
    harness.check(t2.getShearX(), 3.0);
    harness.check(t2.getShearY(), 2.0);
    harness.check(t2.getType() == AffineTransform.TYPE_GENERAL_TRANSFORM);
  }
  
  private void testConstructor3(TestHarness harness) 
  {
    harness.checkPoint("AffineTransform(double[])");
    double[] d1 = new double[] {1.0, 2.0, 3.0, 4.0, 5.0, 6.0};
    AffineTransform t1 = new AffineTransform(d1);
    harness.check(t1.getScaleX(), 1.0);
    harness.check(t1.getScaleY(), 4.0);
    harness.check(t1.getTranslateX(), 5.0);
    harness.check(t1.getTranslateY(), 6.0);
    harness.check(t1.getShearX(), 3.0);
    harness.check(t1.getShearY(), 2.0);
    harness.check(t1.getType() == AffineTransform.TYPE_GENERAL_TRANSFORM);
    
    double[] d2 = new double[] {1.0, 2.0, 3.0, 4.0};
    AffineTransform t2 = new AffineTransform(d2);
    harness.check(t2.getScaleX(), 1.0);
    harness.check(t2.getScaleY(), 4.0);
    harness.check(t2.getTranslateX(), 0.0);
    harness.check(t2.getTranslateY(), 0.0);
    harness.check(t2.getShearX(), 3.0);
    harness.check(t2.getShearY(), 2.0);
    harness.check(t2.getType() == AffineTransform.TYPE_GENERAL_TRANSFORM);
  }

  private void testConstructor4(TestHarness harness) 
  {
    harness.checkPoint("AffineTransform(double, double, double, double, double, double)");
    AffineTransform t1 = new AffineTransform(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
    harness.check(t1.getScaleX(), 1.0);
    harness.check(t1.getScaleY(), 4.0);
    harness.check(t1.getTranslateX(), 5.0);
    harness.check(t1.getTranslateY(), 6.0);
    harness.check(t1.getShearX(), 3.0);
    harness.check(t1.getShearY(), 2.0);
    harness.check(t1.getType() == AffineTransform.TYPE_GENERAL_TRANSFORM);
  }
  
  private void testConstructor5(TestHarness harness) 
  {
    harness.checkPoint("AffineTransform(float[])");
    float[] f1 = new float[] {1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f};
    AffineTransform t1 = new AffineTransform(f1);
    harness.check(t1.getScaleX(), 1.0f);
    harness.check(t1.getScaleY(), 4.0f);
    harness.check(t1.getTranslateX(), 5.0f);
    harness.check(t1.getTranslateY(), 6.0f);
    harness.check(t1.getShearX(), 3.0f);
    harness.check(t1.getShearY(), 2.0f);
    harness.check(t1.getType() == AffineTransform.TYPE_GENERAL_TRANSFORM);
    
    float[] d2 = new float[] {1.0f, 2.0f, 3.0f, 4.0f};
    AffineTransform t2 = new AffineTransform(d2);
    harness.check(t2.getScaleX(), 1.0f);
    harness.check(t2.getScaleY(), 4.0f);
    harness.check(t2.getTranslateX(), 0.0);
    harness.check(t2.getTranslateY(), 0.0);
    harness.check(t2.getShearX(), 3.0f);
    harness.check(t2.getShearY(), 2.0f);
    harness.check(t2.getType() == AffineTransform.TYPE_GENERAL_TRANSFORM);
  }

  private void testConstructor6(TestHarness harness) 
  {
    harness.checkPoint("AffineTransform(float, float, float, float, float, float)");
    AffineTransform t1 = new AffineTransform(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f);
    harness.check(t1.getScaleX(), 1.0f);
    harness.check(t1.getScaleY(), 4.0f);
    harness.check(t1.getTranslateX(), 5.0f);
    harness.check(t1.getTranslateY(), 6.0f);
    harness.check(t1.getShearX(), 3.0f);
    harness.check(t1.getShearY(), 2.0f);
    harness.check(t1.getType() == AffineTransform.TYPE_GENERAL_TRANSFORM);
  }
}
