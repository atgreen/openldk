// MouseListenerTest.java -- 

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
  * Check if {@link MouseListener} could be registered for an AWT {@link Label}
  * and if action is performed when some of mouse labels is pressed.
  */
public class MouseListenerTest
    extends Panel
    implements Testlet
{
  /**
   * Generated serial version UID.
   */
  private static final long serialVersionUID = 477052346645513905L;

  /**
   * These flags are set by MouseListener
   */
  private boolean mouseClickedLabel1Flag = false;
  private boolean mouseClickedLabel2Flag = false;
  private boolean mouseClickedLabel3Flag = false;
  private boolean mousePressedLabel1Flag = false;
  private boolean mousePressedLabel2Flag = false;
  private boolean mousePressedLabel3Flag = false;
  private boolean mouseReleasedLabel1Flag = false;
  private boolean mouseReleasedLabel2Flag = false;
  private boolean mouseReleasedLabel3Flag = false;
  private boolean mouseEnteredFlag = false;
  private boolean mouseExitedFlag = false;

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
    Label label = new Label("xyzzy");
    label.setBackground(Color.blue);
    add(label);

    // register new mouse listener
    label.addMouseListener(
      new MouseListener() {

        public void mouseClicked(MouseEvent e) 
        {
          // figure out which label is pressed
          switch (e.getButton())
          {
            case MouseEvent.BUTTON1: setMouseClickedLabel1Flag(true); break;
            case MouseEvent.BUTTON2: setMouseClickedLabel2Flag(true); break;
            case MouseEvent.BUTTON3: setMouseClickedLabel3Flag(true); break;
            default: break;
          }
        }

        public void mouseEntered(MouseEvent e) 
        {
          setMouseEnteredFlag(true);
        }

        public void mouseExited(MouseEvent e) 
        {
          setMouseExitedFlag(true);
        }

        public void mousePressed(MouseEvent e) 
        {
          // figure out which label is pressed
          switch (e.getButton())
          {
            case MouseEvent.BUTTON1: setMousePressedLabel1Flag(true); break;
            case MouseEvent.BUTTON2: setMousePressedLabel2Flag(true); break;
            case MouseEvent.BUTTON3: setMousePressedLabel3Flag(true); break;
            default: break;
          }
        }

        public void mouseReleased(MouseEvent e) 
        {
          // figure out which label was pressed
          switch (e.getButton())
          {
            case MouseEvent.BUTTON1: setMouseReleasedLabel1Flag(true); break;
            case MouseEvent.BUTTON2: setMouseReleasedLabel2Flag(true); break;
            case MouseEvent.BUTTON3: setMouseReleasedLabel3Flag(true); break;
            default: break;
          }
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

    // check mouse & label behavior using ALL mouse labels
    for (int mouseLabel : new Integer[] {InputEvent.BUTTON1_MASK, InputEvent.BUTTON2_MASK, InputEvent.BUTTON3_MASK})
    {
        // move the mouse cursor outside the label
        robot.mouseMove(0, 0);
        robot.waitForIdle();

        // move the mouse cursor to a tested pixel to show users what's checked
        robot.mouseMove(checkedPixelX, checkedPixelY);
        robot.waitForIdle();
        robot.delay(250);

        // click = press + release
        robot.mousePress(mouseLabel);
        robot.delay(250);
        robot.mouseRelease(mouseLabel);
        robot.delay(250);

        // move the mouse cursor outside the label
        robot.mouseMove(0, 0);
        robot.delay(250);
    }

    // There is a delay to avoid any race conditions    
    // and so user can see frame
    robot.waitForIdle();
    robot.delay(1000);

    // it's necessary to clean up the component from desktop
    frame.dispose();

    // check if all actions were correctly performed
    harness.check(this.isMouseClickedLabel1Flag());
    harness.check(this.isMouseClickedLabel2Flag());
    harness.check(this.isMouseClickedLabel3Flag());
    harness.check(this.isMousePressedLabel1Flag());
    harness.check(this.isMousePressedLabel2Flag());
    harness.check(this.isMousePressedLabel3Flag());
    harness.check(this.isMouseReleasedLabel1Flag());
    harness.check(this.isMouseReleasedLabel2Flag());
    harness.check(this.isMouseReleasedLabel3Flag());
    harness.check(this.isMouseEnteredFlag());
    harness.check(this.isMouseExitedFlag());
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
   * @param mouseEnteredFlag the mouseEnteredFlag to set
   */
  public void setMouseEnteredFlag(boolean mouseEnteredFlag) {
    this.mouseEnteredFlag = mouseEnteredFlag;
  }

  /**
   * @return the mouseEnteredFlag
   */
  public boolean isMouseEnteredFlag() {
    return this.mouseEnteredFlag;
  }

  /**
   * @param mouseExitedFlag the mouseExitedFlag to set
   */
  public void setMouseExitedFlag(boolean mouseExitedFlag) {
    this.mouseExitedFlag = mouseExitedFlag;
  }

  /**
   * @return the mouseExitedFlag
   */
  public boolean isMouseExitedFlag() {
    return this.mouseExitedFlag;
  }

  /**
   * @param mousePressedLabel1Flag the mousePressedLabel1Flag to set
   */
  public void setMousePressedLabel1Flag(boolean mousePressedLabel1Flag) {
    this.mousePressedLabel1Flag = mousePressedLabel1Flag;
  }

  /**
   * @return the mousePressedLabel1Flag
   */
  public boolean isMousePressedLabel1Flag() {
    return this.mousePressedLabel1Flag;
  }

  /**
   * @param mousePressedLabel2Flag the mousePressedLabel2Flag to set
   */
  public void setMousePressedLabel2Flag(boolean mousePressedLabel2Flag) {
    this.mousePressedLabel2Flag = mousePressedLabel2Flag;
  }

  /**
   * @return the mousePressedLabel2Flag
   */
  public boolean isMousePressedLabel2Flag() {
    return this.mousePressedLabel2Flag;
  }

  /**
   * @param mousePressedLabel3Flag the mousePressedLabel3Flag to set
   */
  public void setMousePressedLabel3Flag(boolean mousePressedLabel3Flag) {
    this.mousePressedLabel3Flag = mousePressedLabel3Flag;
  }

  /**
   * @return the mousePressedLabel3Flag
   */
  public boolean isMousePressedLabel3Flag() {
    return this.mousePressedLabel3Flag;
  }

  /**
   * @param mouseClickedLabel1Flag the mouseClickedLabel1Flag to set
   */
  public void setMouseClickedLabel1Flag(boolean mouseClickedLabel1Flag) {
    this.mouseClickedLabel1Flag = mouseClickedLabel1Flag;
  }

  /**
   * @return the mouseClickedLabel1Flag
   */
  public boolean isMouseClickedLabel1Flag() {
    return this.mouseClickedLabel1Flag;
  }

  /**
   * @param mouseClickedLabel2Flag the mouseClickedLabel2Flag to set
   */
  public void setMouseClickedLabel2Flag(boolean mouseClickedLabel2Flag) {
    this.mouseClickedLabel2Flag = mouseClickedLabel2Flag;
  }

  /**
   * @return the mouseClickedLabel2Flag
   */
  public boolean isMouseClickedLabel2Flag() {
    return this.mouseClickedLabel2Flag;
  }

  /**
   * @param mouseClickedLabel3Flag the mouseClickedLabel3Flag to set
   */
  public void setMouseClickedLabel3Flag(boolean mouseClickedLabel3Flag) {
    this.mouseClickedLabel3Flag = mouseClickedLabel3Flag;
  }

  /**
   * @return the mouseClickedLabel3Flag
   */
  public boolean isMouseClickedLabel3Flag() {
    return this.mouseClickedLabel3Flag;
  }

  /**
   * @param mouseReleasedLabel1Flag the mouseReleasedLabel1Flag to set
   */
  public void setMouseReleasedLabel1Flag(boolean mouseReleasedLabel1Flag) {
    this.mouseReleasedLabel1Flag = mouseReleasedLabel1Flag;
  }

  /**
   * @return the mouseReleasedLabel1Flag
   */
  public boolean isMouseReleasedLabel1Flag() {
    return this.mouseReleasedLabel1Flag;
  }

  /**
   * @param mouseReleasedLabel2Flag the mouseReleasedLabel2Flag to set
   */
  public void setMouseReleasedLabel2Flag(boolean mouseReleasedLabel2Flag) {
    this.mouseReleasedLabel2Flag = mouseReleasedLabel2Flag;
  }

  /**
   * @return the mouseReleasedLabel2Flag
   */
  public boolean isMouseReleasedLabel2Flag() {
    return this.mouseReleasedLabel2Flag;
  }

  /**
   * @param mouseReleasedLabel3Flag the mouseReleasedLabel3Flag to set
   */
  public void setMouseReleasedLabel3Flag(boolean mouseReleasedLabel3Flag) {
    this.mouseReleasedLabel3Flag = mouseReleasedLabel3Flag;
  }

  /**
   * @return the mouseReleasedLabel3Flag
   */
  public boolean isMouseReleasedLabel3Flag() {
    return this.mouseReleasedLabel3Flag;
  }
}
