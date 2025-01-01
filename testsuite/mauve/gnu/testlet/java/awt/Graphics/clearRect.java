/* clearRect.java -- checks for the clearRect() method in the Graphics class.
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

package gnu.testlet.java.awt.Graphics;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics2D;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;

public class clearRect implements Testlet
{
  
  public void test(TestHarness harness)
  {
    BufferedImage img = new BufferedImage(20, 20, BufferedImage.TYPE_INT_ARGB);
    Graphics2D g2d = (Graphics2D)img.getGraphics();

    // The default, at least for BufferedImageGraphics
    harness.check(g2d.getBackground(), new Color(0,0,0,255));
    harness.check(img.getRaster().getSample(10, 10, 0), 0);
    harness.check(img.getRaster().getSample(10, 10, 1), 0);
    harness.check(img.getRaster().getSample(10, 10, 2), 0);
    harness.check(img.getRaster().getSample(10, 10, 3), 0);
    
    // Show that clearRect only depends on background, and ignores current
    // color, composite, and paint
    g2d.setColor(new Color(200, 200, 200, 50));
    g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f));
    g2d.setPaint(new GradientPaint(new Point2D.Double(0,0),
                                   new Color(10, 10, 10),
                                   new Point2D.Double(20, 20),
                                   new Color(75, 75, 75),
                                   true));
    
    Color c = new Color(100, 100, 100);
    g2d.setBackground(c);
    g2d.clearRect(0, 0, 20, 20);
    harness.check(img.isAlphaPremultiplied(), false);
    
    harness.check(g2d.getBackground(), c);
    harness.check(img.getRaster().getSample(10, 10, 0), 100);
    harness.check(img.getRaster().getSample(10, 10, 1), 100);
    harness.check(img.getRaster().getSample(10, 10, 2), 100);
    harness.check(img.getRaster().getSample(10, 10, 3), 255);
    
    // Show that the background is applied with a AlphaComposite.SRC rule,
    // that is, the entire contents are directly replaced by the background
    // and no compositing is performed.
    c = new Color(200, 200, 200, 51);
    g2d.setBackground(c);
    g2d.clearRect(0, 0, 20, 20);
    harness.check(img.isAlphaPremultiplied(), false);

    harness.check(g2d.getBackground(), c);
    harness.check(img.getRaster().getSample(10, 10, 0), 200);
    harness.check(img.getRaster().getSample(10, 10, 1), 200);
    harness.check(img.getRaster().getSample(10, 10, 2), 200);
    harness.check(img.getRaster().getSample(10, 10, 3), 51);
  }
}

