// MouseMotionListenerTest.java -- 

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

package gnu.testlet.java.awt.Canvas;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.java.awt.LocationTests;

import java.awt.*;
import java.awt.event.*;

/**
  * Check if MouseMotionListener could be registered for an AWT Canvas
  * and if action is performed when some of mouse buttons is pressed.
  */
public class MouseMotionListenerTest
    extends Panel
    implements Testlet
{
  // these flags are set by MouseMotionListener
  boolean mouseDraggedCanvas1Flag = false;
  boolean mouseDraggedCanvas2Flag = false;
  boolean mouseDraggedCanvas3Flag = false;
  boolean mouseMovedCanvas1Flag = false;
  boolean mouseMovedCanvas2Flag = false;
  boolean mouseMovedCanvas3Flag = false;

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    setBackground(Color.red);
    Frame frame = new Frame();
    Canvas canvas = new Canvas();
    canvas.setBackground(Color.blue);
    canvas.setSize(100,100);
    add(canvas);

    // register new mouse motion listener
    canvas.addMouseMotionListener(
      new MouseMotionListener() {

        public void mouseDragged(MouseEvent e) 
        {
          // figure out which button is pressed
          int modifiers = e.getModifiersEx();
          mouseDraggedCanvas1Flag |= (modifiers & MouseEvent.BUTTON1_DOWN_MASK) != 0;
          mouseDraggedCanvas2Flag |= (modifiers & MouseEvent.BUTTON2_DOWN_MASK) != 0;
          mouseDraggedCanvas3Flag |= (modifiers & MouseEvent.BUTTON3_DOWN_MASK) != 0;
        }

        public void mouseMoved(MouseEvent e) 
        {
          // figure out which button is pressed
          int modifiers = e.getModifiersEx();
          // none of the modifiers should be set
          mouseMovedCanvas1Flag |= (modifiers & MouseEvent.BUTTON1_DOWN_MASK) != 0;
          mouseMovedCanvas2Flag |= (modifiers & MouseEvent.BUTTON2_DOWN_MASK) != 0;
          mouseMovedCanvas3Flag |= (modifiers & MouseEvent.BUTTON3_DOWN_MASK) != 0;
        }

      }
    );

    frame.add(this);
    frame.pack();
    frame.show();

    // AWT robot is used performing some actions
    // also to wait for all
    // widgets to stabilize theirs size and position.
    Robot robot = harness.createRobot();

    // we should wait a moment before the computations
    // and pixel checks
    robot.waitForIdle();
    robot.delay(1000);

    // compute absolute coordinations of canvas on a screen
    Rectangle bounds = canvas.getBounds();
    Point loc = frame.getLocationOnScreen();
    Insets i = frame.getInsets();
    bounds.x += i.left + loc.x;
    bounds.y += i.top + loc.y;

    // position of checked pixel
    int checkedPixelX = bounds.x + bounds.width / 2;
    int checkedPixelY = bounds.y + bounds.height / 2;

    // check mouse & canvas behaviour using ALL mouse buttons
    for (int mouseCanvas : new Integer[] {InputEvent.BUTTON1_MASK, InputEvent.BUTTON2_MASK, InputEvent.BUTTON3_MASK})
    {
        // move the mouse cursor outside the canvas
        robot.mouseMove(0, 0);
        robot.waitForIdle();

        // move the mouse cursor to a tested pixel to show users what's checked
        robot.mouseMove(checkedPixelX - 20, checkedPixelY);
        robot.waitForIdle();
        robot.delay(250);
        robot.mousePress(mouseCanvas);
        robot.delay(250);
        robot.mouseMove(checkedPixelX + 20, checkedPixelY);
        robot.delay(250);
        robot.mouseRelease(mouseCanvas);
        robot.delay(250);
        robot.mouseMove(checkedPixelX - 20, checkedPixelY);
        robot.delay(250);
        robot.mouseMove(checkedPixelX + 20, checkedPixelY);
        robot.delay(250);

        // move the mouse cursor outside the canvas
        robot.mouseMove(0, 0);
        robot.delay(250);
    }

    // There is a delay to avoid any race conditions    
    // and so user can see frame
    robot.waitForIdle();
    robot.delay(1000);

    // it's necesarry to clean up the component from desktop
    frame.dispose();

    // check if all actions were performed
    harness.check(mouseDraggedCanvas1Flag);
    harness.check(mouseDraggedCanvas2Flag);
    harness.check(mouseDraggedCanvas3Flag);

    // negative tests
    harness.check(!mouseMovedCanvas1Flag);
    harness.check(!mouseMovedCanvas2Flag);
    harness.check(!mouseMovedCanvas3Flag);
  }

  /**
    * Paint method for our implementation of a Panel
    */
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

