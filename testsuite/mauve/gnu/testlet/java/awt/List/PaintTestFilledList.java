// PaintTest.java --

// Copyright (C) 2011 Pavel Tisnovsky <ptisnovs@redhat.com>

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
// the Free Software Foundation, Inc., 51 Franklin Street,
// Fifth Floor, Boston, MA 02110-1301 USA.

// Tags: GUI, JDK 1.4

package gnu.testlet.java.awt.List;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Insets;
import java.awt.List;
import java.awt.Panel;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Robot;

/**
 * Test if filled {@link List} could be painted correctly.
 */
public class PaintTestFilledList
    extends Panel
    implements Testlet
{

  /**
   * Generated serial version UID.
   */
  private static final long serialVersionUID = -6498456149126246476L;

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    setBackground(Color.red);
    Frame frame = new Frame();
    List list = new List(10);
    list.setBackground(Color.blue);
    for (int i = 0; i < 10; i++) {
        list.add("item # " + i);
    }
    add(list);
    frame.add(this);
    frame.pack();
    frame.setVisible(true);

    // AWT robot is used for reading pixel colors
    // from a screen and also to wait for all
    // widgets to stabilize theirs size and position.
    Robot robot = harness.createRobot();

    // we should wait a moment before the computations
    // and pixel checks
    robot.waitForIdle();
    robot.delay(1000);

    Point loc = frame.getLocationOnScreen();
    Rectangle bounds = list.getBounds();
    Insets i = frame.getInsets();
    bounds.x += loc.x + i.left;
    bounds.y += loc.y + i.top;

    // position of checked pixel
    int checkedPixelX = bounds.x + bounds.width * 2 / 3;
    int checkedPixelY = bounds.y + bounds.height / 2 + 5;

    // move the mouse cursor to a tested pixel to show users what's checked
    robot.mouseMove(checkedPixelX, checkedPixelY);
    robot.waitForIdle();

    // check the color of a pixel located in the button center
    Color labelColor = robot.getPixelColor(checkedPixelX, checkedPixelY);
    harness.check(labelColor.equals(Color.blue));
    
    // There is a delay to avoid any race conditions    
    // and so user can see frame
    robot.waitForIdle();
    robot.delay(1000);

    // it's necessary to clean up the component from desktop
    frame.dispose();
  }

  @Override
  public void paint(Graphics graphics)
  {
    Image offScr = createImage(getSize().width, getSize().height);
    Graphics offG = offScr.getGraphics();
    offG.setClip(0, 0, getSize().width, getSize().height);

    super.paint(offG);
    graphics.drawImage(offScr, 0, 0, null);

    offG.dispose();
  }
}
