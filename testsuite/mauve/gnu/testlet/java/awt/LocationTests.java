/* LocationTests.java -- 
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

// Tags: JDK 1.4

package gnu.testlet.java.awt;

import java.awt.Color;
import java.awt.Rectangle;
import java.awt.Robot;

import gnu.testlet.TestHarness;

/** 
 * This class tests the location of a rectangle on the screen
 * using the colors of the rectangle and a given color.
 * 
 * @author langel (langel at redhat dot com)
 */
public class LocationTests
{
  
  /**
   * Compares two colors.
   * 
   * @param a -
   *          Color to compare.
   * @param b -
   *          Color to compare.
   * @param match -
   *          true if colors should be equivalent.
   */
  public static void checkColor(TestHarness h, Color a, Color b, boolean match)
  {
    if (match)
      h.check(a.getRed() == b.getRed() && a.getGreen() == b.getGreen()
            && a.getBlue() == b.getBlue());
    else
      h.check(!(a.getRed() == b.getRed() && a.getGreen() == b.getGreen() 
          && a.getBlue() == b.getBlue()));
  }
  
  /**
   * This method checks that the pixels outside of the rectange, at all corners,
   * match (or don't match) a given color.
   * 
   * @param r -
   *          the Robot to use to get the pixel color at a location.
   * @param rect -
   *          the Rectangle to check
   * @param comp -
   *          the Color to compare the pixel colors to.
   * @param match -
   *          true if the pixel outside the rectangle corner should be
   *          equivalent to comp.
   */
  public static void checkRectangleOuterColors(TestHarness h, Robot r, Rectangle rect, Color comp,
                                        boolean match)
  {
    Color c;

    // top-left-left
    c = r.getPixelColor(rect.x - 1, rect.y);
    checkColor(h, c, comp, match);

    // top-left-top
    r.getPixelColor(rect.x, rect.y - 1);
    checkColor(h, c, comp, match);

    // top-right-right
    r.getPixelColor((rect.x + rect.width - 1) + 1, rect.y);
    checkColor(h, c, comp, match);

    // top-right-top
    r.getPixelColor((rect.x + rect.width - 1), rect.y - 1);
    checkColor(h, c, comp, match);

    // bottom-left-left
    r.getPixelColor(rect.x - 1, (rect.y + rect.height - 1));
    checkColor(h, c, comp, match);

    // bottom-left-bottom
    r.getPixelColor(rect.x, (rect.y + rect.height - 1) + 1);
    checkColor(h, c, comp, match);

    // bottom-right-right
    r.getPixelColor((rect.x + rect.width - 1) + 1, (rect.y + rect.height - 1));
    checkColor(h, c, comp, match);

    // bottom-right-bottom
    r.getPixelColor((rect.x + rect.width - 1), (rect.y + rect.height - 1) + 1);
    checkColor(h, c, comp, match);
  }

  /**
   * This method checks the pixel colors of a Rectangle's corners.
   * 
   * @param r -
   *          the Robot to use to get the pixel colors.
   * @param rect -
   *          the Rectangle to check.
   * @param comp -
   *          the Color to compare.
   * @param match -
   *          true if the corner pixel color of the rectangle should be
   *          equivalent to comp.
   */
  public static void checkRectangleCornerColors(TestHarness h, Robot r, Rectangle rect, Color comp,
                                         boolean match)
  {
    Color c;

    // top-left
    c = r.getPixelColor(rect.x, rect.y);
    checkColor(h, c, comp, match);

    // top-right
    c = r.getPixelColor(rect.x + rect.width - 1, rect.y);
    checkColor(h, c, comp, match);

    // bottom-left
    c = r.getPixelColor(rect.x, rect.y + rect.height - 1);
    checkColor(h, c, comp, match);

    // bottom-right
    c = r.getPixelColor(rect.x + rect.width - 1, rect.y + rect.height - 1);
    checkColor(h, c, comp, match);
  }
}
