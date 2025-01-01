/* getSubimage.java -- some checks for the getSubimage() method in the
       BufferedImage class.
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

// Tags: JDK1.4

package gnu.testlet.java.awt.image.BufferedImage;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.BufferedImage;
import java.awt.image.RasterFormatException;

public class getSubimage implements Testlet 
{
  public void test(TestHarness harness)
  {
    // Create initial image, fill with data
    BufferedImage img = new BufferedImage(50, 50, BufferedImage.TYPE_INT_RGB);
    
    for (int x = 0; x < 50; x++)
      for (int y = 0; y < 50; y++)
        for (int b = 0; b < 3; b++)
          img.getRaster().setSample(x, y, b, x+y+b);
    
    // Subimage with identical bounds
    BufferedImage img2 = img.getSubimage(0, 0, 50, 50);
    
    harness.check(img.getRaster() != img2.getRaster());
    harness.check(img.getRaster().getDataBuffer(),
                  img2.getRaster().getDataBuffer());
    harness.check(img.getSampleModel(), img2.getSampleModel());
    harness.check(img.getColorModel(), img2.getColorModel());
    harness.check(img2.getMinX(), 0);
    harness.check(img2.getMinY(), 0);
    harness.check(img2.getWidth(), 50);
    harness.check(img2.getHeight(), 50);
    
    for (int x = 0; x < 50; x++)
      for (int y = 0; y < 50; y++)
        for (int b = 0; b < 3; b++)
          harness.check(img2.getRaster().getSample(x, y, b), x+y+b);
    
    // Offset subimage with smaller bounds
    img2 = img.getSubimage(20, 30, 15, 10);
    
    harness.check(img.getRaster().getDataBuffer(),
                  img2.getRaster().getDataBuffer());
    harness.check(img.getSampleModel(), img2.getSampleModel());
    harness.check(img2.getMinX(), 0);
    harness.check(img2.getMinY(), 0);
    harness.check(img2.getWidth(), 15);
    harness.check(img2.getHeight(), 10);
    
    for (int x = 0; x < 15; x++)
      for (int y = 0; y < 10; y++)
        for (int b = 0; b < 3; b++)
          harness.check(img2.getRaster().getSample(x, y, b), x+20 + y+30 +b);
    
    // Subimage overflows original image bounds
    try
    {
      img2 = img.getSubimage(40, 40, 20, 20);
      harness.check(false);
    }
    catch (RasterFormatException ex)
    {
      harness.check(true);
    }
  }
}
