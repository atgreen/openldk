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

package gnu.testlet.java.awt.TextArea;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.*;
import java.awt.event.*;

/**
  * Check if MouseMotionListener could be registered for an AWT TextArea
  * and if action is performed when some of mouse button is pressed.
  */
public class MouseMotionListenerTest
    extends Panel
    implements Testlet
{
  /**
   * Generated serial version UID.
   */
  private static final long serialVersionUID = 4009815071982906794L;

  // these flags are set by MouseMotionListener
  private boolean mouseDraggedButton1Flag = false;
  private boolean mouseDraggedButton2Flag = false;
  private boolean mouseDraggedButton3Flag = false;
  private boolean mouseMovedButton1Flag = false;
  private boolean mouseMovedButton2Flag = false;
  private boolean mouseMovedButton3Flag = false;

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  @SuppressWarnings("boxing")
  public void test(TestHarness harness)
  {
    setBackground(Color.red);
    Frame frame = new Frame();
    TextArea textArea = new TextArea("xyzzy");
    textArea.setBackground(Color.blue);
    add(textArea);

    // register new mouse motion listener
    textArea.addMouseMotionListener(
      new MouseMotionListener() {

        public void mouseDragged(MouseEvent e) 
        {
          // figure out which mouse button is pressed
          int modifiers = e.getModifiersEx();
          setMouseDraggedButton1Flag(isMouseDraggedButton1Flag()
              | ((modifiers & InputEvent.BUTTON1_DOWN_MASK) != 0));
          setMouseDraggedButton2Flag(isMouseDraggedButton2Flag()
              | ((modifiers & InputEvent.BUTTON2_DOWN_MASK) != 0));
          setMouseDraggedButton3Flag(isMouseDraggedButton3Flag()
              | ((modifiers & InputEvent.BUTTON3_DOWN_MASK) != 0));
        }

        public void mouseMoved(MouseEvent e) 
        {
          // figure out which mouse button is pressed
          int modifiers = e.getModifiersEx();
          // none of the modifiers should be set
          setMouseMovedButton1Flag(isMouseMovedButton1Flag()
              | ((modifiers & InputEvent.BUTTON1_DOWN_MASK) != 0));
          setMouseMovedButton2Flag(isMouseMovedButton2Flag()
              | ((modifiers & InputEvent.BUTTON2_DOWN_MASK) != 0));
          setMouseMovedButton3Flag(isMouseMovedButton3Flag()
              | ((modifiers & InputEvent.BUTTON3_DOWN_MASK) != 0));
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

    // compute absolute coordinations of textArea on a screen
    Rectangle bounds = textArea.getBounds();
    Point loc = frame.getLocationOnScreen();
    Insets i = frame.getInsets();
    bounds.x += i.left + loc.x;
    bounds.y += i.top + loc.y;

    // position of checked pixel
    int checkedPixelX = bounds.x + bounds.width / 2;
    int checkedPixelY = bounds.y + bounds.height / 2;

    // check mouse & text area behavior using ALL mouse buttons
    for (int mouseButton : new Integer[] {InputEvent.BUTTON1_MASK, InputEvent.BUTTON2_MASK, InputEvent.BUTTON3_MASK})
    {
        // move the mouse cursor outside the text area
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

        // move the mouse cursor outside the text area
        robot.mouseMove(0, 0);
        robot.delay(250);
    }

    // There is a delay to avoid any race conditions    
    // and so user can see frame
    robot.waitForIdle();
    robot.delay(1000);

    // it's necessary to clean up the component from desktop
    frame.dispose();

    // check if all actions were performed
    harness.check(isMouseDraggedButton1Flag());
    harness.check(isMouseDraggedButton2Flag());
    harness.check(isMouseDraggedButton3Flag());

    // negative tests
    harness.check(!isMouseMovedButton1Flag());
    harness.check(!isMouseMovedButton2Flag());
    harness.check(!isMouseMovedButton3Flag());
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
   * @param mouseDraggedButton1Flag the mouseDraggedButton1Flag to set
   */
  public void setMouseDraggedButton1Flag(boolean mouseDraggedButton1Flag) {
    this.mouseDraggedButton1Flag = mouseDraggedButton1Flag;
  }

  /**
   * @return the mouseDraggedButton1Flag
   */
  public boolean isMouseDraggedButton1Flag() {
    return this.mouseDraggedButton1Flag;
  }

  /**
   * @param mouseDraggedButton2Flag the mouseDraggedButton2Flag to set
   */
  public void setMouseDraggedButton2Flag(boolean mouseDraggedButton2Flag) {
    this.mouseDraggedButton2Flag = mouseDraggedButton2Flag;
  }

  /**
   * @return the mouseDraggedButton2Flag
   */
  public boolean isMouseDraggedButton2Flag() {
    return this.mouseDraggedButton2Flag;
  }

  /**
   * @param mouseDraggedButton3Flag the mouseDraggedButton3Flag to set
   */
  public void setMouseDraggedButton3Flag(boolean mouseDraggedButton3Flag) {
    this.mouseDraggedButton3Flag = mouseDraggedButton3Flag;
  }

  /**
   * @return the mouseDraggedButton3Flag
   */
  public boolean isMouseDraggedButton3Flag() {
    return this.mouseDraggedButton3Flag;
  }

  /**
   * @param mouseMovedButton1Flag the mouseMovedButton1Flag to set
   */
  public void setMouseMovedButton1Flag(boolean mouseMovedButton1Flag) {
    this.mouseMovedButton1Flag = mouseMovedButton1Flag;
  }

  /**
   * @return the mouseMovedButton1Flag
   */
  public boolean isMouseMovedButton1Flag() {
    return this.mouseMovedButton1Flag;
  }

  /**
   * @param mouseMovedButton2Flag the mouseMovedButton2Flag to set
   */
  public void setMouseMovedButton2Flag(boolean mouseMovedButton2Flag) {
    this.mouseMovedButton2Flag = mouseMovedButton2Flag;
  }

  /**
   * @return the mouseMovedButton2Flag
   */
  public boolean isMouseMovedButton2Flag() {
    return this.mouseMovedButton2Flag;
  }

  /**
   * @param mouseMovedButton3Flag the mouseMovedButton3Flag to set
   */
  public void setMouseMovedButton3Flag(boolean mouseMovedButton3Flag) {
    this.mouseMovedButton3Flag = mouseMovedButton3Flag;
  }

  /**
   * @return the mouseMovedButton3Flag
   */
  public boolean isMouseMovedButton3Flag() {
    return this.mouseMovedButton3Flag;
  }
}

