/* transform.java -- some checks for the transform() method in the Graphics2D
       class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
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

package gnu.testlet.java.awt.Graphics2D;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;

public class transform implements Testlet
{
  public void test(TestHarness harness)
  {
    testBufferedImageGraphics2D(harness);
  }
  
  /**
   * Checks for the Graphics2D from a BufferedImage.
   * 
   * @param harness  the test harness.
   */
  public void testBufferedImageGraphics2D(TestHarness harness)
  {
    harness.checkPoint("BufferedImage Graphics2D");
    BufferedImage image = new BufferedImage(100, 80, 
            BufferedImage.TYPE_INT_ARGB);
    Graphics2D g2 = image.createGraphics();
    harness.check(g2.getTransform().isIdentity());
    g2.setTransform(new AffineTransform(1.0, 2.0, 3.0, 4.0, 5.0, 6.0));
    g2.transform(new AffineTransform(7.0, 8.0, 9.0, 10.0, 11.0, 12.0));
    AffineTransform current = g2.getTransform();
    harness.check(current.getScaleX(), 31.0);
    harness.check(current.getScaleY(), 58.0);
    harness.check(current.getShearX(), 39.0);
    harness.check(current.getShearY(), 46.0);
    harness.check(current.getTranslateX(), 52.0);
    harness.check(current.getTranslateY(), 76.0);

    // here we check what happens to the clip shape when a transform is
    // applied...
    g2.setTransform(new AffineTransform());
    g2.setClip(1, 2, 3, 4);
    Shape currentClip = g2.getClip();
    harness.check(currentClip, new Rectangle2D.Double(1, 2, 3, 4));
    g2.transform(AffineTransform.getTranslateInstance(10.0, 20.0));
    currentClip = g2.getClip();
    harness.check(currentClip, new Rectangle2D.Double(-9, -18, 3, 4));
  }
}
