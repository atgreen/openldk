// Tags: JDK1.0

/* SimpleGrabber.java
   Copyright (C) 2003 Red Hat, Inc.

   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
 
   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
 
   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA. */

package gnu.testlet.java.awt.image.PixelGrabber;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.*;
import java.awt.image.*;

public class SimpleGrabber implements Testlet
{
  TestHarness harness;

  public void test (TestHarness harness)
  {
    this.harness = harness;

    Image i = null;

    try
      {
	String f = "gnu#testlet#java#awt#image#PixelGrabber#lena1.jpg";
	i = Toolkit.getDefaultToolkit().getImage (harness.getResourceFile (f).getAbsolutePath ());
      }
    catch (Exception e)
      {
	harness.fail ("lena1.jpg not found.");
      }

    String str = getPixels (i, 0, 0, 6, 12);

    harness.check (str,
		  "Pixel (0, 0)\n"
		  + "   R: 98 G: 31 B: 82\n"
		  + "Pixel (1, 0)\n"
		  + "   R: 79 G: 12 B: 63\n"
		  + "Pixel (2, 0)\n"
		  + "   R: 76 G: 9 B: 60\n"
		  + "Pixel (3, 0)\n"
		  + "   R: 152 G: 85 B: 136\n"
		  + "Pixel (4, 0)\n"
		  + "   R: 132 G: 66 B: 114\n"
		  + "Pixel (5, 0)\n"
		  + "   R: 95 G: 29 B: 77\n"
		  + "Pixel (0, 1)\n"
		  + "   R: 86 G: 19 B: 70\n"
		  + "Pixel (1, 1)\n"
		  + "   R: 75 G: 8 B: 59\n"
		  + "Pixel (2, 1)\n"
		  + "   R: 81 G: 14 B: 65\n"
		  + "Pixel (3, 1)\n"
		  + "   R: 150 G: 83 B: 134\n"
		  + "Pixel (4, 1)\n"
		  + "   R: 121 G: 55 B: 103\n"
		  + "Pixel (5, 1)\n"
		  + "   R: 85 G: 19 B: 67\n"
		  + "Pixel (0, 2)\n"
		  + "   R: 75 G: 11 B: 61\n"
		  + "Pixel (1, 2)\n"
		  + "   R: 79 G: 15 B: 65\n"
		  + "Pixel (2, 2)\n"
		  + "   R: 100 G: 36 B: 86\n"
		  + "Pixel (3, 2)\n"
		  + "   R: 155 G: 91 B: 141\n"
		  + "Pixel (4, 2)\n"
		  + "   R: 115 G: 52 B: 99\n"
		  + "Pixel (5, 2)\n"
		  + "   R: 76 G: 13 B: 60\n"
		  + "Pixel (0, 3)\n"
		  + "   R: 70 G: 8 B: 57\n"
		  + "Pixel (1, 3)\n"
		  + "   R: 91 G: 29 B: 78\n"
		  + "Pixel (2, 3)\n"
		  + "   R: 122 G: 60 B: 109\n"
		  + "Pixel (3, 3)\n"
		  + "   R: 162 G: 100 B: 149\n"
		  + "Pixel (4, 3)\n"
		  + "   R: 116 G: 53 B: 100\n"
		  + "Pixel (5, 3)\n"
		  + "   R: 69 G: 6 B: 53\n"
		  + "Pixel (0, 4)\n"
		  + "   R: 72 G: 13 B: 61\n"
		  + "Pixel (1, 4)\n"
		  + "   R: 102 G: 43 B: 91\n"
		  + "Pixel (2, 4)\n"
		  + "   R: 128 G: 69 B: 117\n"
		  + "Pixel (3, 4)\n"
		  + "   R: 154 G: 95 B: 143\n"
		  + "Pixel (4, 4)\n"
		  + "   R: 120 G: 58 B: 105\n"
		  + "Pixel (5, 4)\n"
		  + "   R: 64 G: 2 B: 49\n"
		  + "Pixel (0, 5)\n"
		  + "   R: 95 G: 36 B: 84\n"
		  + "Pixel (1, 5)\n"
		  + "   R: 117 G: 58 B: 106\n"
		  + "Pixel (2, 5)\n"
		  + "   R: 119 G: 60 B: 108\n"
		  + "Pixel (3, 5)\n"
		  + "   R: 136 G: 77 B: 125\n"
		  + "Pixel (4, 5)\n"
		  + "   R: 133 G: 74 B: 120\n"
		  + "Pixel (5, 5)\n"
		  + "   R: 73 G: 14 B: 60\n"
		  + "Pixel (0, 6)\n"
		  + "   R: 123 G: 65 B: 113\n"
		  + "Pixel (1, 6)\n"
		  + "   R: 125 G: 67 B: 115\n"
		  + "Pixel (2, 6)\n"
		  + "   R: 93 G: 35 B: 83\n"
		  + "Pixel (3, 6)\n"
		  + "   R: 106 G: 48 B: 96\n"
		  + "Pixel (4, 6)\n"
		  + "   R: 146 G: 89 B: 134\n"
		  + "Pixel (5, 6)\n"
		  + "   R: 88 G: 31 B: 76\n"
		  + "Pixel (0, 7)\n"
		  + "   R: 137 G: 79 B: 127\n"
		  + "Pixel (1, 7)\n"
		  + "   R: 123 G: 65 B: 113\n"
		  + "Pixel (2, 7)\n"
		  + "   R: 65 G: 7 B: 55\n"
		  + "Pixel (3, 7)\n"
		  + "   R: 78 G: 20 B: 68\n"
		  + "Pixel (4, 7)\n"
		  + "   R: 149 G: 92 B: 137\n"
		  + "Pixel (5, 7)\n"
		  + "   R: 95 G: 38 B: 83\n"
		  + "Pixel (0, 8)\n"
		  + "   R: 138 G: 80 B: 128\n"
		  + "Pixel (1, 8)\n"
		  + "   R: 96 G: 38 B: 86\n"
		  + "Pixel (2, 8)\n"
		  + "   R: 78 G: 20 B: 68\n"
		  + "Pixel (3, 8)\n"
		  + "   R: 76 G: 18 B: 66\n"
		  + "Pixel (4, 8)\n"
		  + "   R: 106 G: 49 B: 94\n"
		  + "Pixel (5, 8)\n"
		  + "   R: 122 G: 65 B: 110\n"
		  + "Pixel (0, 9)\n"
		  + "   R: 155 G: 97 B: 145\n"
		  + "Pixel (1, 9)\n"
		  + "   R: 99 G: 41 B: 89\n"
		  + "Pixel (2, 9)\n"
		  + "   R: 73 G: 15 B: 63\n"
		  + "Pixel (3, 9)\n"
		  + "   R: 80 G: 22 B: 70\n"
		  + "Pixel (4, 9)\n"
		  + "   R: 115 G: 58 B: 103\n"
		  + "Pixel (5, 9)\n"
		  + "   R: 107 G: 50 B: 95\n"
		  + "Pixel (0, 10)\n"
		  + "   R: 165 G: 106 B: 154\n"
		  + "Pixel (1, 10)\n"
		  + "   R: 99 G: 40 B: 88\n"
		  + "Pixel (2, 10)\n"
		  + "   R: 68 G: 9 B: 57\n"
		  + "Pixel (3, 10)\n"
		  + "   R: 83 G: 24 B: 72\n"
		  + "Pixel (4, 10)\n"
		  + "   R: 123 G: 64 B: 110\n"
		  + "Pixel (5, 10)\n"
		  + "   R: 88 G: 29 B: 75\n"
		  + "Pixel (0, 11)\n"
		  + "   R: 151 G: 92 B: 140\n"
		  + "Pixel (1, 11)\n"
		  + "   R: 93 G: 34 B: 82\n"
		  + "Pixel (2, 11)\n"
		  + "   R: 68 G: 9 B: 57\n"
		  + "Pixel (3, 11)\n"
		  + "   R: 81 G: 22 B: 70\n"
		  + "Pixel (4, 11)\n"
		  + "   R: 120 G: 58 B: 105\n"
		  + "Pixel (5, 11)\n"
		  + "   R: 74 G: 12 B: 59\n");
  }

  // Test eight-parameter constructor.
  public String getPixels(Image img, int x, int y, int w, int h)
  {
    int[] pix = new int[w * h];

    PixelGrabber pg = new PixelGrabber(img, x, y, w, h, pix, 0, w);
    try
      {
	pg.grabPixels(5000);
      }
    catch (InterruptedException e)
      {
	harness.fail ("image production interrupted.");
	return "";
      }

    if ((pg.getStatus() & ImageObserver.ABORT) != 0)
      {
	harness.fail ("image production aborted.");
	return "";
      }

    String result = "";
    for (int j = 0; j < h; j++)
      {
	for (int i = 0; i < w; i++)
	  {
	    int p = j * w + i;
	    result = result + getPixel (x + i, y + j, pix[p]);
	  }
      }
    return result;
  }

  public String getPixel (int x, int y, int pixel)
  {
    ColorModel model = ColorModel.getRGBdefault();

    return "Pixel (" + x + ", " + y + ")\n  "
      + " R: " + model.getRed (pixel)
      + " G: " + model.getGreen (pixel)
      + " B: " + model.getBlue (pixel) + "\n";
  }
}
