/* getPoint2D.java -- some checks for the getPoint2D) method of the
              ColorConvertOp class.
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

package gnu.testlet.java.awt.image.ColorConvertOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.color.ColorSpace;
import java.awt.geom.Point2D;
import java.awt.image.ColorConvertOp;

/**
 * Checks the getPoint2D method in the
 * {@link ColorConvertOp} class.
 */
public class getPoint2D implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("getPoint2D");
    
    //  This is a simple test; the Op should not change the
    // geometry of the raster
    
    ColorConvertOp op = new ColorConvertOp(ColorSpace.getInstance(ColorSpace.CS_sRGB),
                                           null);
    Point2D dest = null;
    dest = op.getPoint2D(new Point2D.Double(3, 3), dest);
    harness.check(dest, new Point2D.Double(3, 3));
  }
}

