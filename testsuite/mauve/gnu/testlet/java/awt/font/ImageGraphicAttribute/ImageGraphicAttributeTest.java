/* ImageGraphicAttributeTest.java --
   Copyright (C) 2006 Red Hat
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

// Tags: JDK1.4

package gnu.testlet.java.awt.font.ImageGraphicAttribute;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Image;
import java.awt.font.ImageGraphicAttribute;
import java.awt.geom.Rectangle2D;
import java.io.File;

import javax.imageio.ImageIO;

public class ImageGraphicAttributeTest
    implements Testlet
{
  
  public void test(TestHarness harness)
  {
    try
      {
        Image image = ImageIO.read(new File("gnu/testlet/java/awt/font/ImageGraphicAttribute/image.bmp"));
        ImageGraphicAttribute iga = new ImageGraphicAttribute(image, 0);

        harness.check(iga.hashCode(), image.hashCode());
        harness.check(iga.getAscent(), 0.0);
        harness.check(iga.getDescent(), 64.0);
        harness.check(iga.getAdvance(), 127.0);
        harness.check(iga.getBounds(), new Rectangle2D.Float((float) - 0.0,
                                                             (float) - 0.0,
                                                             (float) 127.0,
                                                             (float) 64.0));
        
        ImageGraphicAttribute iga2 = new ImageGraphicAttribute(image, 0, 10, -1202);

        harness.check(iga2.hashCode(), image.hashCode());
        harness.check(iga2.getAscent(), 0.0);
        harness.check(iga2.getDescent(), 1266.0);
        harness.check(iga2.getAdvance(), 117.0);
        harness.check(iga2.getBounds(), new Rectangle2D.Float((float) - 10.0,
                                                             (float) 1202.0,
                                                             (float) 127.0,
                                                             (float) 64.0));
        
        harness.check(iga.equals(iga2), false);
      }
    catch (Exception e)
    {
      harness.fail("Exception caught: " + e);
    }
  }
}
