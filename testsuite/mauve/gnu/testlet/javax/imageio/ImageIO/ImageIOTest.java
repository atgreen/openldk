// Tags: JDK1.4

// Copyright (C) 2004  Michael Koch <konqueror@gmx.de>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.


package gnu.testlet.javax.imageio.ImageIO;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.BufferedImage;
import java.awt.image.PixelGrabber;
import java.io.File;

import javax.imageio.ImageIO;

/**
 * @author Michael Koch (konqueror@gmx.de)
 * @author Lillian Angel (langel at redhat dot com)
 * @author Pavel Tisnovsky (ptisnovs at redhat dot com)
 */
public class ImageIOTest
  implements Testlet
{
  public void test(TestHarness h)
  {
    testStringData(h);
    
    // Tests the reading/writing of various images.
    h.checkPoint("Reading and writing of images.");
    testReadWrite(h, "gnu/testlet/javax/imageio/ImageIO/Bitmap-32Bit.bmp");
    testReadWrite(h, "gnu/testlet/javax/imageio/ImageIO/Bitmap-24Bit.bmp");
    testReadWrite(h, "gnu/testlet/javax/imageio/ImageIO/Bitmap-16Bit.bmp");
    testReadWrite(h, "gnu/testlet/javax/imageio/ImageIO/Bitmap-8Bit.bmp");
    testReadWrite(h, "gnu/testlet/javax/imageio/ImageIO/Bitmap-4Bit.bmp");
    testReadWrite(h, "gnu/testlet/javax/imageio/ImageIO/Bitmap-1Bit.bmp");
    testReadWrite(h, "gnu/testlet/javax/imageio/ImageIO/Bitmap-RLE8.bmp");
    testReadWrite(h, "gnu/testlet/javax/imageio/ImageIO/Bitmap-RLE4.bmp");

    // Test also working with PNG file format
    testReadWrite(h, "gnu/testlet/javax/imageio/ImageIO/Bitmap-1Bit.png");
    testReadWrite(h, "gnu/testlet/javax/imageio/ImageIO/Bitmap-4Bit.png");
    testReadWrite(h, "gnu/testlet/javax/imageio/ImageIO/Bitmap-8Bit.png");
    testReadWrite(h, "gnu/testlet/javax/imageio/ImageIO/Bitmap-24Bit.png");
    testReadWrite(h, "gnu/testlet/javax/imageio/ImageIO/Bitmap-32Bit.png");
  }
  
  private void testStringData(TestHarness h)
  {
    String[] stringData;

    // check #1: getReaderFormatNames
    stringData = ImageIO.getReaderFormatNames();
    h.check(stringData.length != 0, "empty reader format names");

    // check #2: getReaderMIMETypes
    stringData = ImageIO.getReaderMIMETypes();
    h.check(stringData.length != 0, "empty reader mime types");

    // check #3: getWriterFormatNames
    stringData = ImageIO.getWriterFormatNames();
    h.check(stringData.length != 0, "empty writer format names");

    // check #4: getWriterMIMETypes
    stringData = ImageIO.getWriterMIMETypes();
    h.check(stringData.length != 0, "empty writer mime types");
  }

  private void testReadWrite(TestHarness h, String picPath)
  {
    boolean exceptionCaught = false;
    try
      {
        BufferedImage image = ImageIO.read(new File(picPath));
        
        int width = image.getWidth(null);
        int height = image.getHeight(null);
        int size = width * height;
        int[] pixels = new int[size];
        int[] outPixels = new int[size];
        String path = "gnu/testlet/javax/imageio/ImageIO/outputBitmap.bmp";

        // do pixel-by-pixel check only if proper writer is found and used
        if (ImageIO.write(image, "bmp", new File(path)))
          {
            BufferedImage outImage = ImageIO.read(new File(path));
            PixelGrabber pg1 = new PixelGrabber(outImage,
                                                0, 0, width, height, outPixels, 0,
                                                width);
            PixelGrabber pg2 = new PixelGrabber(image, 0, 0, width, height, pixels,
                                                0, width);
            pg1.grabPixels();
            pg2.grabPixels();

            h.check(comparePixels(pixels, outPixels, size));
          }
      }
    catch (Exception e)
      {
        exceptionCaught = true;
      }

    h.check(! exceptionCaught);
  }
  
  private boolean comparePixels(int[] a, int[] b, int size)
  {
    for (int i = 0; i < size; i++)
      if (a[i] != b[i])
        return false;
    return true;
  }
}
