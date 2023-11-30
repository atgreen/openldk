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
 *
import java.awt.*;
 *
 *
 */



/**
 * Checks that RoundRectangle2D.intersects works correctly.  In the
 * illustration below, rectangles considered <em>intersecting</em> are
 * painted green, and rectangles considered <em>non-intersecting</em>
 * are painted red.
 *
 * <p><img src="doc-files/intersects-1.png" width="286" height="277"
 * alt="A visualization of the tested rectangles" />
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class intersects
  implements Testlet
{
  RoundRectangle2D rr = new RoundRectangle2D.Double(0,0,10,10,6,6);

  private static final double[] coords = { -3, -1, 0.5, 4, 6, 9.5, 11, 13 };

  // Tests for bug #6067 in GNU Classpath
  public void test(TestHarness harness)
  {
    int ck = 0;
    for (int y = 0; y < coords.length - 1; y++)
      for (int x = 0; x < coords.length - 1; x++)
        harness.check(rr.intersects(coords[x], coords[y],
                                    coords[x + 1] - coords[x],
                                    coords[y + 1] - coords[y])
                      == getExpected(++ck));
  }


  private boolean getExpected(int check)
  {
    return ((check >= 10) && (check <= 12))
      || ((check >= 16) && (check <= 20))
      || ((check >= 23) && (check <= 27))
      || ((check >= 30) && (check <= 34))
      || ((check >= 38) && (check <= 40));
  }


  /* This code was used to generate the image.
   *
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
        g2.scale(14,14);
        g2.translate(5, 5);
        g2.fill(rr);

        Rectangle2D rect = new Rectangle2D.Double();
        g2.setFont(new Font("Lucida", Font.BOLD, 1).deriveFont(0.8f));
        int ck = 0;
        for (int y = 0; y < coords.length - 1; y++)
        {
          for (int x = 0; x < coords.length - 1; x++)
          {
            double w, h;

            w = coords[x + 1] - coords[x];
            h = coords[y + 1] - coords[y];

            if (getExpected(++ck))
              g2.setColor(new Color(0,255,0,120));
            else
              g2.setColor(new Color(255,0,0,120));
            
            rect.setRect(coords[x], coords[y], w, h);
            g2.fill(rect);
            g2.setColor(new Color(255,255,255,128));
            g2.setStroke(new BasicStroke(0.05f));
            g2.draw(rect);
            g2.setColor(Color.BLACK);
            g2.drawString(String.valueOf(ck), (float) coords[x] + 0.15f,
                          (float) coords[y] + 1.15f);
            g2.setColor(Color.WHITE);
            g2.drawString(String.valueOf(ck), (float) coords[x] + 0.1f,
                          (float) coords[y] + 1.1f);
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
    new intersects().showWindow();
  }
  */
}
