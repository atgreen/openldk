/* getPoint2D.java -- some checks for the getPoint2D() method of the
              AffineTransformOp class.
   Copyright (C) 2006 Francis Kung <fkung@redhat.com>
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

// Tags: JDK1.2

package gnu.testlet.java.awt.image.AffineTransformOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.image.AffineTransformOp;

/**
 * Checks the getPoint2D method in the
 * {@link AffineTransformOp} class.
 */
public class getPoint2D implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)      
  {
    testIdentity(harness);
    testRotation(harness);
    testScale(harness);
    testShear(harness);
    testTranslation(harness);
  }
  
  private void testIdentity(TestHarness harness)
  {
    harness.checkPoint("testIdentity");

    AffineTransform xform = new AffineTransform();
    AffineTransformOp op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    harness.check(op.getPoint2D(new Point2D.Double(5, 5), null), new Point2D.Double(5,5));
    
    Point2D pt = null;
    Point2D pt2 = op.getPoint2D(new Point2D.Double(10,-5), pt);
    harness.check(pt, null);        // this is what the ref impl does...
    harness.check(pt2, new Point2D.Double(10, -5));
    
    pt = new Point2D.Double(0,0);
    pt2 = op.getPoint2D(new Point2D.Double(10,-5), pt);
    harness.check(pt, new Point2D.Double(10, -5));
    harness.check(pt, pt2);
    
    pt = new Point2D.Float(0,0);
    op.getPoint2D(new Point2D.Float(-10,-5), pt);
    harness.check(pt, new Point2D.Float(-10, -5));
  }
  
  private void testRotation(TestHarness harness)
  {
    harness.checkPoint("testRotation");

    AffineTransform xform = AffineTransform.getRotateInstance(Math.PI / 2);
    AffineTransformOp op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    harness.check(op.getPoint2D(new Point2D.Double(5, 5), null), new Point2D.Double(-5,5));

    // Do it again, but result in a diamond (not another level rectangle)
    xform.rotate(Math.PI / 3);
    op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    harness.check(op.getPoint2D(new Point2D.Double(5, 5), null), new Point2D.Double(-6.830127018922193, -1.8301270189221923));
    
    // Rotation about a point
    xform.setToIdentity();
    xform.rotate(Math.PI / 2, 10, 2);
    op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    harness.check(op.getPoint2D(new Point2D.Double(5, 5), null), new Point2D.Double(7,-3));
  }
  
  private void testScale(TestHarness harness)
  {
    harness.checkPoint("testScale");
    
    AffineTransform xform = AffineTransform.getScaleInstance(1.0, 1.0);
    AffineTransformOp op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    harness.check(op.getPoint2D(new Point2D.Double(5, 5), null), new Point2D.Double(5, 5));
    
    xform.scale(2.5, 4.75);
    op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    harness.check(op.getPoint2D(new Point2D.Double(5, 5), null), new Point2D.Double(12.5,23.75));
  }
  
  private void testShear(TestHarness harness)
  {
    harness.checkPoint("testShear");
    
    AffineTransform xform = AffineTransform.getShearInstance(1.5, 3.25);
    AffineTransformOp op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    harness.check(op.getPoint2D(new Point2D.Double(5, 5), null), new Point2D.Double(12.5,21.25));
  }
  
  private void testTranslation(TestHarness harness)
  {
    harness.checkPoint("testTranslation");

    AffineTransform xform = AffineTransform.getTranslateInstance(75, 50);
    AffineTransformOp op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    harness.check(op.getPoint2D(new Point2D.Double(5, 5), null), new Point2D.Double(80,55));
  }
}

