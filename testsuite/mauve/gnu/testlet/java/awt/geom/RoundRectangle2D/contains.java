// Tags: JDK1.2

// Copyright (C) 2003 Sascha Brawer <brawer@dandelis.ch>

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

package gnu.testlet.java.awt.geom.RoundRectangle2D;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.geom.RoundRectangle2D;


/* Uncomment this line and the code towards the end of the class
 * for displaying a visualization of the testlet.
 *

import java.awt.*;

 *
 */



/**
 * Checks that RoundRectangle2D.contains works correctly.  In the
 * illustration below, points considered <em>inside</em> are painted
 * green, and points considered <em>outside</em> are painted red.
 *
 * <p><img src="doc-files/contains-1.png" width="198" height="184"
 * alt="A visualization of the tested points" />
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class contains
  implements Testlet
{
  RoundRectangle2D rr = new RoundRectangle2D.Double(0,0,10,10,6,6);

  private static final double[] coords = { -1, 0.5, 5, 9.5, 11 };

  public void test(TestHarness harness)
  {
    for (int y = 0; y < coords.length; y++)
      for (int x = 0; x < coords.length; x++)
        harness.check(rr.contains(coords[x], coords[y])
                      == getExpected(coords[x], coords[y]));
  }


  private boolean getExpected(double x, double y)
  {
    if ((x == 0.5) && (y == 5))
      return true;

    if ((x == 5) && ((y == 0.5) || (y == 5) || (y == 9.5)))
      return true;

    if ((x == 9.5) && (y == 5))
      return true;

    return false;
  }


  /* This code was used to generate the image.
   *
   *

  public void showWindow()
  {
    Frame f = new Frame("Foo");
    Canvas c = new Canvas()
    {
      public void paint(Graphics g)
      {
        Graphics2D g2 = (Graphics2D) g;
        g2.setBackground(Color.WHITE);
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                            RenderingHints.VALUE_ANTIALIAS_ON);
        g2.clearRect(0,0,getWidth(),getHeight());
        g2.scale(10,10);
        g2.translate(4, 4);
        g2.fill(rr);

        Rectangle2D rect = new Rectangle2D.Double();

        for (int y = 0; y < coords.length; y++)
        {
          for (int x = 0; x < coords.length; x++)
          {
            if (getExpected(coords[x], coords[y]))
              g2.setColor(Color.GREEN);
            else
              g2.setColor(Color.RED);

            rect.setRect(coords[x], coords[y], 0.2, 0.2);
            g2.fill(rect);
          }
        }
      }
    };

    f.add(c);
    f.pack();
    f.setSize(new Dimension(200, 200));
    f.show();
  }

  public static void main(String[] args)
  {
    new contains().showWindow();
  }
  */
}
