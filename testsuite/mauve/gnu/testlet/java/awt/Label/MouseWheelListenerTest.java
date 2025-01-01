// MouseWheelListenerTest.java -- 

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

// Tags: GUI
// Uses: ../LocationTests

package gnu.testlet.java.awt.Label;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.*;
import java.awt.event.*;

/**
  * Check if MouseWheelListener could be registered for an AWT Label
  * and if action is performed when mouse wheel is rotated up and down.
  */
public class MouseWheelListenerTest
    extends Panel
    implements Testlet
{
  /**
   * Generated serial version UID.
   */
  private static final long serialVersionUID = -4774724125231540431L;
  // these flags are set by MouseWheelListener
  private boolean mouseWheelScrollUpFlag = false;
  private boolean mouseWheelScrollDownFlag = false;

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    setBackground(Color.red);
    Frame frame = new Frame();
    Label label = new Label("xyzzy");
    label.setBackground(Color.blue);
    add(label);

    // register new mouse wheel listener
    label.addMouseWheelListener(
      new MouseWheelListener() {

        public void mouseWheelMoved(MouseWheelEvent e)
        {
          // figure out if mouse wheel is scrolled up or down
          setMouseWheelScrollUpFlag(isMouseWheelScrollUpFlag()
              | (e.getWheelRotation() < 0));
          setMouseWheelScrollDownFlag(isMouseWheelScrollDownFlag()
              | (e.getWheelRotation() > 0));
        }

      }
    );

    frame.add(this);
    frame.pack();
    frame.setVisible(true);

    // AWT robot is used performing some actions
    // also to wait for all
    // widgets to stabilize theirs size and position.
    Robot robot = harness.createRobot();

    // we should wait a moment before the computations
    // and pixel checks
    robot.waitForIdle();
    robot.delay(1000);

    // compute absolute coordinations of label on a screen
    Rectangle bounds = label.getBounds();
    Point loc = frame.getLocationOnScreen();
    Insets i = frame.getInsets();
    bounds.x += i.left + loc.x;
    bounds.y += i.top + loc.y;

    // position of checked pixel
    int checkedPixelX = bounds.x + bounds.width / 2;
    int checkedPixelY = bounds.y + bounds.height / 2;

    // move the mouse cursor to a tested pixel to show users what's checked
    robot.mouseMove(checkedPixelX, checkedPixelY);
    robot.waitForIdle();
    robot.delay(250);
    robot.mouseWheel(+1);
    robot.delay(250);
    robot.mouseWheel(-1);
    robot.delay(250);

    // There is a delay to avoid any race conditions    
    // and so user can see frame
    robot.waitForIdle();
    robot.delay(1000);

    // it's necessary to clean up the component from desktop
    frame.dispose();

    // check if all actions were performed
    harness.check(isMouseWheelScrollUpFlag());
    harness.check(isMouseWheelScrollDownFlag());
  }

  /**
    * Paint method for our implementation of a Panel
    */
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

  /**
   * @param mouseWheelScrollUpFlag the mouseWheelScrollUpFlag to set
   */
  public void setMouseWheelScrollUpFlag(boolean mouseWheelScrollUpFlag) {
    this.mouseWheelScrollUpFlag = mouseWheelScrollUpFlag;
  }

  /**
   * @return the mouseWheelScrollUpFlag
   */
  public boolean isMouseWheelScrollUpFlag() {
    return this.mouseWheelScrollUpFlag;
  }

  /**
   * @param mouseWheelScrollDownFlag the mouseWheelScrollDownFlag to set
   */
  public void setMouseWheelScrollDownFlag(boolean mouseWheelScrollDownFlag) {
    this.mouseWheelScrollDownFlag = mouseWheelScrollDownFlag;
  }

  /**
   * @return the mouseWheelScrollDownFlag
   */
  public boolean isMouseWheelScrollDownFlag() {
    return this.mouseWheelScrollDownFlag;
  }
}

