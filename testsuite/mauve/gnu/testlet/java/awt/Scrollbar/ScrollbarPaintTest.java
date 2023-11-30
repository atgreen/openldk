/* ScrollbarPaintTest.java -- 
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

// Tags: GUI, JDK 1.0

package gnu.testlet.java.awt.Scrollbar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Panel;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.Scrollbar;

public class ScrollbarPaintTest
    extends Panel
    implements Testlet
{

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    setBackground(Color.red);
    Frame f = new Frame();
    Scrollbar c = new Scrollbar();
    add(c);
    f.add(this);
    f.pack();
    f.show();

    // AWT robot is used for reading pixel colors
    // from a screen and also to wait for all
    // widgets to stabilize theirs size and position.
    Robot r = harness.createRobot();
    r.delay(1000);

    // we should wait a moment before the computations
    // and pixel checks
    r.waitForIdle();

    Rectangle bounds = c.getBounds();

    Point loc = f.getLocationOnScreen();
    Insets i = f.getInsets();
    loc.x += i.left + bounds.x;
    loc.y += i.top + bounds.y;
    bounds.x += i.left;
    bounds.y += i.top;
    bounds.width -= (i.left + i.right);
    bounds.height -= (i.top + i.bottom);
    
    // check the color of a pixel located in the button center
    Color scroll = r.getPixelColor(loc.x + bounds.width, loc.y + bounds.height/2);

    // Check if scrollbar was painted.
    harness.check(!(scroll.equals(Color.red)));
    
    // There is a delay to avoid any race conditions    
    // and so user can see frame
    r.waitForIdle();
    r.delay(1000);

    // it's necesarry to clean up
    f.dispose();
  }

  public void paint(Graphics g)
  {
    Image offScr = createImage(getSize().width, getSize().height);
    Graphics offG = offScr.getGraphics();
    offG.setClip(0, 0, getSize().width, getSize().height);

    super.paint(offG);
    g.drawImage(offScr, 0, 0, null);

    offG.dispose();
  }
}

