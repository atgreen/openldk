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

import java.awt.image.ColorModel;

/**
 * Some tests for the getComponentSize() method in the {@link ColorModel} class.
 */
public class getComponentSize implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    test1(harness);
    test2(harness);
  }

  public void test1(TestHarness harness)      
  {
    harness.checkPoint("()");
    ColorModel rgb = ColorModel.getRGBdefault();
    int[] sizes = rgb.getComponentSize();
    harness.check(sizes.length, 4);
    harness.check(sizes[0], 8);
    harness.check(sizes[1], 8);
    harness.check(sizes[2], 8);
    harness.check(sizes[3], 8);
  }

  public void test2(TestHarness harness)      
  {
    harness.checkPoint("(int)");
    ColorModel rgb = ColorModel.getRGBdefault();
    harness.check(rgb.getComponentSize(0), 8);
    harness.check(rgb.getComponentSize(1), 8);
    harness.check(rgb.getComponentSize(2), 8);
    harness.check(rgb.getComponentSize(3), 8);
    
    boolean pass = false;
    try
    {
      rgb.getComponentSize(-1);
    }
    catch (ArrayIndexOutOfBoundsException e) {
      pass = true;
    }
    harness.check(pass);

    pass = false;
    try
    {
      rgb.getComponentSize(4);
    }
    catch (ArrayIndexOutOfBoundsException e) {
      pass = true;
    }
    harness.check(pass);
  }
  
}
