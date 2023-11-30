/* ShapeGraphicAttributeTest.java -- 
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

package gnu.testlet.java.awt.font.ShapeGraphicAttribute;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Shape;
import java.awt.font.ShapeGraphicAttribute;
import java.awt.geom.Rectangle2D;

public class ShapeGraphicAttributeTest
    implements Testlet
{

  public void test(TestHarness harness)
  {
    try
      {
        Shape s = new Rectangle2D.Float(2, 3, 60, 98);
        ShapeGraphicAttribute sga = new ShapeGraphicAttribute(s, 0, true);

        harness.check(sga.hashCode(), s.hashCode());
        harness.check(sga.getAscent(), 0.0);
        harness.check(sga.getDescent(), 101.0);
        harness.check(sga.getAdvance(), 62.0);
        harness.check(sga.getBounds(), new Rectangle2D.Float((float) 2.0,
                                                             (float) 3.0,
                                                             (float) 61.0,
                                                             (float) 99.0));

        Shape s2 = new Rectangle2D.Float(2, - 5, 17, 921);
        ShapeGraphicAttribute sga2 = new ShapeGraphicAttribute(s2, 1, false);

        harness.check(sga2.hashCode(), s2.hashCode());
        harness.check(sga2.getAscent(), 5.0);
        harness.check(sga2.getDescent(), 916.0);
        harness.check(sga2.getAdvance(), 19.0);
        harness.check(sga2.getBounds(), new Rectangle2D.Float((float) 2.0,
                                                              (float) - 5.0,
                                                              (float) 17.0,
                                                              (float) 921.0));

        harness.check(sga.equals(sga2), false);
      }
    catch (Exception e)
      {
        harness.fail("Exception caught: " + e);
      }
  }
}
