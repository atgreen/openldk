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

package gnu.testlet.java.awt.Button;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.java.awt.LocationTests;

import java.awt.*;
import java.awt.event.*;

/**
  * Check if MouseMotionListener could be registered for an AWT Button
  * and if action is performed when some of mouse buttons is pressed.
  */
public class MouseMotionListenerTest
    extends Panel
    implements Testlet
{
  // these flags are set by MouseMotionListener
  boolean mouseDraggedButton1Flag = false;
  boolean mouseDraggedButton2Flag = false;
  boolean mouseDraggedButton3Flag = false;
  boolean mouseMovedButton1Flag = false;
  boolean mouseMovedButton2Flag = false;
  boolean mouseMovedButton3Flag = false;

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    setBackground(Color.red);
    Frame frame = new Frame();
    Button button = new Button("xyzzy");
    button.setBackground(Color.blue);
    add(button);

    // register new mouse motion listener
    button.addMouseMotionListener(
      new MouseMotionListener() {

        public void mouseDragged(MouseEvent e) 
        {
          // figure out which button is pressed
          int modifiers = e.getModifiersEx();
          mouseDraggedButton1Flag |= (modifiers & MouseEvent.BUTTON1_DOWN_MASK) != 0;
          mouseDraggedButton2Flag |= (modifiers & MouseEvent.BUTTON2_DOWN_MASK) != 0;
          mouseDraggedButton3Flag |= (modifiers & MouseEvent.BUTTON3_DOWN_MASK) != 0;
        }

        public void mouseMoved(MouseEvent e) 
        {
          // figure out which button is pressed
          int modifiers = e.getModifiersEx();
          // none of the modifiers should be set
          mouseMovedButton1Flag |= (modifiers & MouseEvent.BUTTON1_DOWN_MASK) != 0;
          mouseMovedButton2Flag |= (modifiers & MouseEvent.BUTTON2_DOWN_MASK) != 0;
          mouseMovedButton3Flag |= (modifiers & MouseEvent.BUTTON3_DOWN_MASK) != 0;
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

    // compute absolute coordinations of button on a screen
    Rectangle bounds = button.getBounds();
    Point loc = frame.getLocationOnScreen();
    Insets i = frame.getInsets();
    bounds.x += i.left + loc.x;
    bounds.y += i.top + loc.y;

    // position of checked pixel
    int checkedPixelX = bounds.x + bounds.width / 2;
    int checkedPixelY = bounds.y + bounds.height / 2;

    // check mouse & button behaviour using ALL mouse buttons
    for (int mouseButton : new Integer[] {InputEvent.BUTTON1_MASK, InputEvent.BUTTON2_MASK, InputEvent.BUTTON3_MASK})
    {
        // move the mouse cursor outside the button
        robot.mouseMove(0, 0);
        robot.waitForIdle();

        // move the mouse cursor to a tested pixel to show users what's checked
        robot.mouseMove(checkedPixelX - 20, checkedPixelY);
        robot.waitForIdle();
        robot.delay(250);
        robot.mousePress(mouseButton);
        robot.delay(250);
        robot.mouseMove(checkedPixelX + 20, checkedPixelY);
        robot.delay(250);
        robot.mouseRelease(mouseButton);
        robot.delay(250);
        robot.mouseMove(checkedPixelX - 20, checkedPixelY);
        robot.delay(250);
        robot.mouseMove(checkedPixelX + 20, checkedPixelY);
        robot.delay(250);

        // move the mouse cursor outside the button
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
    harness.check(mouseDraggedButton1Flag);
    harness.check(mouseDraggedButton2Flag);
    harness.check(mouseDraggedButton3Flag);

    // negative tests
    harness.check(!mouseMovedButton1Flag);
    harness.check(!mouseMovedButton2Flag);
    harness.check(!mouseMovedButton3Flag);
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

