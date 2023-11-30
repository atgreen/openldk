/* getBounds2D.java -- some checks for the getBounds2D() method of the
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
import java.awt.geom.Rectangle2D;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;

/**
 * Checks the getBounds2D method in the
 * {@link AffineTransformOp} class.
 */
public class getBounds2D implements Testlet 
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
    BufferedImage img = new BufferedImage(30, 40, BufferedImage.TYPE_INT_RGB);
    harness.check(op.getBounds2D(img), new Rectangle2D.Float(0, 0, 30, 40));
  }
  
  private void testRotation(TestHarness harness)
  {
    harness.checkPoint("testRotation");

    AffineTransform xform = AffineTransform.getRotateInstance(Math.PI / 2);
    AffineTransformOp op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    BufferedImage img = new BufferedImage(30, 40, BufferedImage.TYPE_INT_RGB);
    harness.check(op.getBounds2D(img), new Rectangle2D.Float(-40, 0, 40, 30));

    // Do it again, but result in a diamond (not another level rectangle)
    xform.rotate(Math.PI / 3);
    op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    harness.check(op.getBounds2D(img), new Rectangle2D.Float(-45.980762f,
                                                             -34.641018f, 
                                                             45.980762f,
                                                             49.641018f));
    
    // Rotation about a point
    xform.setToIdentity();
    xform.rotate(Math.PI / 2, 10, 15);
    op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    harness.check(op.getBounds2D(img), new Rectangle2D.Float(-15, 5, 40, 30));
  }
  
  private void testScale(TestHarness harness)
  {
    harness.checkPoint("testScale");
    
    AffineTransform xform = AffineTransform.getScaleInstance(1.0, 1.0);
    BufferedImage img = new BufferedImage(30, 40, BufferedImage.TYPE_INT_RGB);
    AffineTransformOp op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    harness.check(op.getBounds2D(img), new Rectangle2D.Float(0, 0, 30, 40));
    
    xform.scale(2.5, 4.75);
    op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    harness.check(op.getBounds2D(img), new Rectangle2D.Float(0, 0, 75, 190));   
  }
  
  private void testShear(TestHarness harness)
  {
    harness.checkPoint("testHarness");
    
    AffineTransform xform = AffineTransform.getShearInstance(1.5, 3.25);
    BufferedImage img = new BufferedImage(30, 40, BufferedImage.TYPE_INT_RGB);
    AffineTransformOp op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    harness.check(op.getBounds2D(img), new Rectangle2D.Float(0, 0, 90, 137.5f));
  }
  
  private void testTranslation(TestHarness harness)
  {
    harness.checkPoint("testTranslation");

    AffineTransform xform = AffineTransform.getTranslateInstance(75, 50);
    BufferedImage img = new BufferedImage(30, 40, BufferedImage.TYPE_INT_RGB);
    AffineTransformOp op = new AffineTransformOp(xform, AffineTransformOp.TYPE_BICUBIC);
    harness.check(op.getBounds2D(img), new Rectangle2D.Float(75, 50, 30, 40));
  }
}

