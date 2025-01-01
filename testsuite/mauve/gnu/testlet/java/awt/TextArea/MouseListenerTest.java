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

package gnu.testlet.java.awt.TextArea;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.*;
import java.awt.event.*;

/**
  * Check if {@link MouseListener} could be registered for an AWT {@link TextArea}
  * and if action is performed when some of mouse button is pressed.
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
  private boolean mouseClickedButton1Flag = false;
  private boolean mouseClickedButton2Flag = false;
  private boolean mouseClickedButton3Flag = false;
  private boolean mousePressedButton1Flag = false;
  private boolean mousePressedButton2Flag = false;
  private boolean mousePressedButton3Flag = false;
  private boolean mouseReleasedButton1Flag = false;
  private boolean mouseReleasedButton2Flag = false;
  private boolean mouseReleasedButton3Flag = false;
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
    TextArea textArea = new TextArea("xyzzy");
    textArea.setBackground(Color.blue);
    add(textArea);

    // register new mouse listener
    textArea.addMouseListener(
      new MouseListener() {

        public void mouseClicked(MouseEvent e) 
        {
          // figure out which button is pressed
          switch (e.getButton())
          {
            case MouseEvent.BUTTON1: setMouseClickedButton1Flag(true); break;
            case MouseEvent.BUTTON2: setMouseClickedButton2Flag(true); break;
            case MouseEvent.BUTTON3: setMouseClickedButton3Flag(true); break;
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
          // figure out which mouse button is pressed
          switch (e.getButton())
          {
            case MouseEvent.BUTTON1: setMousePressedButton1Flag(true); break;
            case MouseEvent.BUTTON2: setMousePressedButton2Flag(true); break;
            case MouseEvent.BUTTON3: setMousePressedButton3Flag(true); break;
            default: break;
          }
        }

        public void mouseReleased(MouseEvent e) 
        {
          // figure out which mouse button was pressed
          switch (e.getButton())
          {
            case MouseEvent.BUTTON1: setMouseReleasedButton1Flag(true); break;
            case MouseEvent.BUTTON2: setMouseReleasedButton2Flag(true); break;
            case MouseEvent.BUTTON3: setMouseReleasedButton3Flag(true); break;
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

    // compute absolute coordinations of text area on a screen
    Rectangle bounds = textArea.getBounds();
    Point loc = frame.getLocationOnScreen();
    Insets i = frame.getInsets();
    bounds.x += i.left + loc.x;
    bounds.y += i.top + loc.y;

    // position of checked pixel
    int checkedPixelX = bounds.x + bounds.width / 2;
    int checkedPixelY = bounds.y + bounds.height / 2;

    // check mouse & textArea behavior using ALL mouse buttons
    for (int mouseTextArea : new Integer[] {InputEvent.BUTTON1_MASK, InputEvent.BUTTON2_MASK, InputEvent.BUTTON3_MASK})
    {
        // move the mouse cursor outside the text area
        robot.mouseMove(0, 0);
        robot.waitForIdle();

        // move the mouse cursor to a tested pixel to show users what's checked
        robot.mouseMove(checkedPixelX, checkedPixelY);
        robot.waitForIdle();
        robot.delay(250);

        // click = press + release
        robot.mousePress(mouseTextArea);
        robot.delay(250);
        robot.mouseRelease(mouseTextArea);
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

    // check if all actions were correctly performed
    harness.check(this.isMouseClickedButton1Flag());
    harness.check(this.isMouseClickedButton2Flag());
    harness.check(this.isMouseClickedButton3Flag());
    harness.check(this.isMousePressedButton1Flag());
    harness.check(this.isMousePressedButton2Flag());
    harness.check(this.isMousePressedButton3Flag());
    harness.check(this.isMouseReleasedButton1Flag());
    harness.check(this.isMouseReleasedButton2Flag());
    harness.check(this.isMouseReleasedButton3Flag());
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
   * @param mousePressedButton1Flag the mousePressedButton1Flag to set
   */
  public void setMousePressedButton1Flag(boolean mousePressedButton1Flag) {
    this.mousePressedButton1Flag = mousePressedButton1Flag;
  }

  /**
   * @return the mousePressedButton1Flag
   */
  public boolean isMousePressedButton1Flag() {
    return this.mousePressedButton1Flag;
  }

  /**
   * @param mousePressedButton2Flag the mousePressedButton2Flag to set
   */
  public void setMousePressedButton2Flag(boolean mousePressedButton2Flag) {
    this.mousePressedButton2Flag = mousePressedButton2Flag;
  }

  /**
   * @return the mousePressedButton2Flag
   */
  public boolean isMousePressedButton2Flag() {
    return this.mousePressedButton2Flag;
  }

  /**
   * @param mousePressedButton3Flag the mousePressedButton3Flag to set
   */
  public void setMousePressedButton3Flag(boolean mousePressedButton3Flag) {
    this.mousePressedButton3Flag = mousePressedButton3Flag;
  }

  /**
   * @return the mousePressedButton3Flag
   */
  public boolean isMousePressedButton3Flag() {
    return this.mousePressedButton3Flag;
  }

  /**
   * @param mouseClickedButton1Flag the mouseClickedButton1Flag to set
   */
  public void setMouseClickedButton1Flag(boolean mouseClickedButton1Flag) {
    this.mouseClickedButton1Flag = mouseClickedButton1Flag;
  }

  /**
   * @return the mouseClickedButton1Flag
   */
  public boolean isMouseClickedButton1Flag() {
    return this.mouseClickedButton1Flag;
  }

  /**
   * @param mouseClickedButton2Flag the mouseClickedButton2Flag to set
   */
  public void setMouseClickedButton2Flag(boolean mouseClickedButton2Flag) {
    this.mouseClickedButton2Flag = mouseClickedButton2Flag;
  }

  /**
   * @return the mouseClickedButton2Flag
   */
  public boolean isMouseClickedButton2Flag() {
    return this.mouseClickedButton2Flag;
  }

  /**
   * @param mouseClickedButton3Flag the mouseClickedButton3Flag to set
   */
  public void setMouseClickedButton3Flag(boolean mouseClickedButton3Flag) {
    this.mouseClickedButton3Flag = mouseClickedButton3Flag;
  }

  /**
   * @return the mouseClickedButton3Flag
   */
  public boolean isMouseClickedButton3Flag() {
    return this.mouseClickedButton3Flag;
  }

  /**
   * @param mouseReleasedButton1Flag the mouseReleasedButton1Flag to set
   */
  public void setMouseReleasedButton1Flag(boolean mouseReleasedButton1Flag) {
    this.mouseReleasedButton1Flag = mouseReleasedButton1Flag;
  }

  /**
   * @return the mouseReleasedButton1Flag
   */
  public boolean isMouseReleasedButton1Flag() {
    return this.mouseReleasedButton1Flag;
  }

  /**
   * @param mouseReleasedButton2Flag the mouseReleasedButton2Flag to set
   */
  public void setMouseReleasedButton2Flag(boolean mouseReleasedButton2Flag) {
    this.mouseReleasedButton2Flag = mouseReleasedButton2Flag;
  }

  /**
   * @return the mouseReleasedButton2Flag
   */
  public boolean isMouseReleasedButton2Flag() {
    return this.mouseReleasedButton2Flag;
  }

  /**
   * @param mouseReleasedButton3Flag the mouseReleasedButton3Flag to set
   */
  public void setMouseReleasedButton3Flag(boolean mouseReleasedButton3Flag) {
    this.mouseReleasedButton3Flag = mouseReleasedButton3Flag;
  }

  /**
   * @return the mouseReleasedButton3Flag
   */
  public boolean isMouseReleasedButton3Flag() {
    return this.mouseReleasedButton3Flag;
  }
}
