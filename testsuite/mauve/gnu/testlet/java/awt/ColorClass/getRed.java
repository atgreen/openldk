// Tags: JDK1.1

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.ColorClass;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;

/**
 * Some checks for the getRed() method in the {@link Color} class.  
 */
public class getRed implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    Color c = new Color(1, 2, 3);
    harness.check(c.getRed(), 1);
 
    c = new Color(243, 244, 245);
    harness.check(c.getRed(), 243);

    thoroughTest(harness);
  }

  /**
    * This test checks some RGB combinations, ie. subset of 2^24 colors
    * It tooks 1-5 seconds to complete on a reasonable hardware.
    */
  private void thoroughTest(TestHarness harness)
  {
    for (int red = 0; red < 256; red+=8)
      {
        for (int green = 0; green < 256; green+=8)
          {
            for (int blue = 0; blue < 256; blue+=8)
              {
                Color c = new Color(red, green, blue);
                harness.check(c.getRed(), red);
              }
          }
      }
  }
}
