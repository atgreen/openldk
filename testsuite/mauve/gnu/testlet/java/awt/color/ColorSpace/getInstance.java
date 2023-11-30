// Tags: JDK1.4

// Copyright (C) 2004 Sascha Brawer <brawer@dandelis.ch>

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

package gnu.testlet.java.awt.color.ColorSpace;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.color.ColorSpace;


/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class getInstance
  implements Testlet
{
  public void test(TestHarness h)
  {
    test_CS_sRGB(h);
    test_CS_CIEXYZ(h);
    test_CS_PYCC(h);
    test_CS_GRAY(h);
    test_CS_LINEAR_RGB(h);
    test_TYPE_GRAY(h);
  }


  public void test_CS_sRGB(TestHarness h)
  {
    ColorSpace cs;

    h.checkPoint("CS_sRGB");
    cs = ColorSpace.getInstance(ColorSpace.CS_sRGB);
    h.check(cs.getType(), ColorSpace.TYPE_RGB);  // Check #1.
    h.check(cs.getNumComponents(), 3);           // Check #2.
    checkEpsilon(h, cs.getMinValue(0), 0.0f);    // Check #3.
    checkEpsilon(h, cs.getMaxValue(0), 1.0f);    // Check #4.
    checkEpsilon(h, cs.getMinValue(1), 0.0f);    // Check #5.
    checkEpsilon(h, cs.getMaxValue(1), 1.0f);    // Check #6.
    checkEpsilon(h, cs.getMinValue(2), 0.0f);    // Check #7.
    checkEpsilon(h, cs.getMaxValue(2), 1.0f);    // Check #8.
  }


  public void test_CS_CIEXYZ(TestHarness h)
  {
    ColorSpace cs;

    h.checkPoint("CS_CIEXYZ");
    cs = ColorSpace.getInstance(ColorSpace.CS_CIEXYZ);
    h.check(cs.getType(), ColorSpace.TYPE_XYZ);  // Check #1.
    h.check(cs.getNumComponents(), 3);           // Check #2.
    checkEpsilon(h, cs.getMinValue(0), 0.0f);    // Check #3.
    checkEpsilon(h, cs.getMaxValue(0), 2.0f);    // Check #4.
    checkEpsilon(h, cs.getMinValue(1), 0.0f);    // Check #5.
    checkEpsilon(h, cs.getMaxValue(1), 2.0f);    // Check #6.
    checkEpsilon(h, cs.getMinValue(2), 0.0f);    // Check #7.
    checkEpsilon(h, cs.getMaxValue(2), 2.0f);    // Check #8.
  }


  public void test_CS_PYCC(TestHarness h)
  {
    ColorSpace cs;

    h.checkPoint("CS_PYCC");
    cs = ColorSpace.getInstance(ColorSpace.CS_PYCC);
    h.check(cs.getType(), ColorSpace.TYPE_3CLR); // Check #1.
    h.check(cs.getNumComponents(), 3);           // Check #2.
    checkEpsilon(h, cs.getMinValue(0), 0.0f);    // Check #3.
    checkEpsilon(h, cs.getMaxValue(0), 1.0f);    // Check #4.
    checkEpsilon(h, cs.getMinValue(1), 0.0f);    // Check #5.
    checkEpsilon(h, cs.getMaxValue(1), 1.0f);    // Check #6.
    checkEpsilon(h, cs.getMinValue(2), 0.0f);    // Check #7.
    checkEpsilon(h, cs.getMaxValue(2), 1.0f);    // Check #8.
  }


  public void test_CS_GRAY(TestHarness h)
  {
    ColorSpace cs;

    h.checkPoint("CS_GRAY");
    cs = ColorSpace.getInstance(ColorSpace.CS_GRAY);
    h.check(cs.getType(), ColorSpace.TYPE_GRAY); // Check #1.
    h.check(cs.getNumComponents(), 1);           // Check #2.
    h.check(cs.getMinValue(0), 0.0f);            // Check #3.
    h.check(cs.getMaxValue(0), 1.0f);            // Check #4.
  }


  public void test_CS_LINEAR_RGB(TestHarness h)
  {
    ColorSpace cs;

    h.checkPoint("CS_LINEAR_RGB");
    cs = ColorSpace.getInstance(ColorSpace.CS_LINEAR_RGB);
    h.check(cs.getType(), ColorSpace.TYPE_RGB);  // Check #1.
    h.check(cs.getNumComponents(), 3);           // Check #2.
    checkEpsilon(h, cs.getMinValue(0), 0.0f);    // Check #3.
    checkEpsilon(h, cs.getMaxValue(0), 1.0f);    // Check #4.
    checkEpsilon(h, cs.getMinValue(1), 0.0f);    // Check #5.
    checkEpsilon(h, cs.getMaxValue(1), 1.0f);    // Check #6.
    checkEpsilon(h, cs.getMinValue(2), 0.0f);    // Check #7.
    checkEpsilon(h, cs.getMaxValue(2), 1.0f);    // Check #8.
  }


  public void test_TYPE_GRAY(TestHarness h)
  {
    Throwable caught = null;

    h.checkPoint("TYPE_GRAY");
    try
      {
        ColorSpace.getInstance(ColorSpace.TYPE_GRAY);
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    h.check(caught instanceof IllegalArgumentException); // Check #1.
  }


  private static void checkEpsilon(TestHarness h,
                                   float value,
                                   float expected)
  {
    if (Math.abs(value - expected) < 0.001f)
      h.check(true);
    else
      h.check(value, expected);
  }
}
