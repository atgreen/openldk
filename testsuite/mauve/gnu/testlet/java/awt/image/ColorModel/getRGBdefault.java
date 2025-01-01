// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.image.ColorModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Transparency;
import java.awt.color.ColorSpace;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DirectColorModel;

/**
 * Some tests for the getRGBdefault() method in the {@link ColorModel} class.
 */
public class getRGBdefault implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    ColorModel rgb = ColorModel.getRGBdefault();
    harness.check(rgb instanceof DirectColorModel);      // 1
    harness.check(rgb.getPixelSize(), 32);               // 2
    harness.check(rgb.getNumColorComponents(), 3);       // 3
    harness.check(rgb.getNumComponents(), 4);            // 4
    harness.check(rgb.getComponentSize(0), 8);           // 5
    harness.check(rgb.getComponentSize(1), 8);           // 6
    harness.check(rgb.getComponentSize(2), 8);           // 7
    harness.check(rgb.getComponentSize(3), 8);           // 8
    harness.check(rgb.getTransparency(), Transparency.TRANSLUCENT);  // 9
    harness.check(rgb.getTransferType(), DataBuffer.TYPE_INT);       // 10
    harness.check(rgb.isAlphaPremultiplied(), false);                // 11
    harness.check(rgb.getColorSpace(), ColorSpace.getInstance(ColorSpace.CS_sRGB));  // 12 
  }

}
