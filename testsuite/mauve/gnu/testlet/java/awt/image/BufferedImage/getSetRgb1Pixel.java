/* getSetRgb1Pixel.java -- Tests the work of the single pixel get/set Rgb
 Copyright (C) 2006 Audrius Meskauskas
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

// Tags: 1.4


package gnu.testlet.java.awt.image.BufferedImage;

import java.awt.Color;
import java.awt.image.BufferedImage;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests the getRgb and setRgb.
 * 
 * @author Audrius Meskauskas
 */
public class getSetRgb1Pixel implements Testlet
{

  public void test(TestHarness h)
  {
    testType(h, BufferedImage.TYPE_3BYTE_BGR, "TYPE_3BYTE_BGR");
    testType(h, BufferedImage.TYPE_4BYTE_ABGR, "TYPE_4BYTE_ABGR");
    testType(h, BufferedImage.TYPE_4BYTE_ABGR_PRE, "TYPE_4BYTE_ABGR_PRE");
    testType(h, BufferedImage.TYPE_INT_ARGB, "TYPE_INT_ARGB");
    testType(h, BufferedImage.TYPE_INT_ARGB_PRE, "TYPE_INT_ARGB_PRE");
    testType(h, BufferedImage.TYPE_INT_BGR, "TYPE_INT_BGR");
    testType(h, BufferedImage.TYPE_INT_RGB, "TYPE_INT_RGB");
  }

  public void testType(TestHarness h, int type, String typeName)
  {
    int w = 10;

    BufferedImage b = new BufferedImage(w, w, type);

    Color[][] colors = new Color[w][];

    for (int i = 0; i < colors.length; i++)
      {
        colors[i] = new Color[w];
        for (int j = 0; j < colors[i].length; j++)
          {
            colors[i][j] = new Color((int) (Math.random() * 255),
                                     (int) (Math.random() * 255),
                                     (int) (Math.random() * 255));
            b.setRGB(i, j, colors[i][j].getRGB());
          }
      }

    BufferedImage cloned = new BufferedImage(w, w, type);
    b.copyData(cloned.getRaster());

    for (int i = 0; i < colors.length; i++)
      for (int j = 0; j < colors[i].length; j++)
        {
          if (!colors[i][j].equals(new Color(b.getRGB(i, j))))
            {
              h.fail("Failed " + typeName + ", " + colors[i][j] + " v "
                     + new Color(b.getRGB(i, j)));
              return;
            }
          int blue = Color.blue.getRGB();
          b.setRGB(i, j, blue);
          if (b.getRGB(i, j) != blue)
            {
              h.fail("Failed " + typeName + " when resetting into blue " + blue
                     + " v " + b.getRGB(i, j));
              return;
            }
        }

    for (int i = 0; i < colors.length; i++)
      for (int j = 0; j < colors[i].length; j++)
        {
          if (!colors[i][j].equals(new Color(cloned.getRGB(i, j))))
            {
              h.fail("Failed " + typeName + " on cloned image");
              return;
            }
        }
  }
}
