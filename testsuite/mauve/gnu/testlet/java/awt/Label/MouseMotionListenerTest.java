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

package gnu.testlet.java.awt.Label;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.*;
import java.awt.event.*;

/**
  * Check if MouseMotionListener could be registered for an AWT Label
  * and if action is performed when some of mouse labels is pressed.
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
  private boolean mouseDraggedLabel1Flag = false;
  private boolean mouseDraggedLabel2Flag = false;
  private boolean mouseDraggedLabel3Flag = false;
  private boolean mouseMovedLabel1Flag = false;
  private boolean mouseMovedLabel2Flag = false;
  private boolean mouseMovedLabel3Flag = false;

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
    Label label = new Label("The quick brown fox jumps over the lazy dog.");
    label.setBackground(Color.blue);
    add(label);

    // register new mouse motion listener
    label.addMouseMotionListener(
      new MouseMotionListener() {

        public void mouseDragged(MouseEvent e) 
        {
          // figure out which label is pressed
          int modifiers = e.getModifiersEx();
          setMouseDraggedLabel1Flag(isMouseDraggedLabel1Flag()
              | ((modifiers & InputEvent.BUTTON1_DOWN_MASK) != 0));
          setMouseDraggedLabel2Flag(isMouseDraggedLabel2Flag()
              | ((modifiers & InputEvent.BUTTON2_DOWN_MASK) != 0));
          setMouseDraggedLabel3Flag(isMouseDraggedLabel3Flag()
              | ((modifiers & InputEvent.BUTTON3_DOWN_MASK) != 0));
        }

        public void mouseMoved(MouseEvent e) 
        {
          // figure out which label is pressed
          int modifiers = e.getModifiersEx();
          // none of the modifiers should be set
          setMouseMovedLabel1Flag(isMouseMovedLabel1Flag()
              | ((modifiers & InputEvent.BUTTON1_DOWN_MASK) != 0));
          setMouseMovedLabel2Flag(isMouseMovedLabel2Flag()
              | ((modifiers & InputEvent.BUTTON2_DOWN_MASK) != 0));
          setMouseMovedLabel3Flag(isMouseMovedLabel3Flag()
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
        robot.mouseMove(checkedPixelX - 20, checkedPixelY);
        robot.waitForIdle();
        robot.delay(250);
        robot.mousePress(mouseLabel);
        robot.delay(250);
        robot.mouseMove(checkedPixelX + 20, checkedPixelY);
        robot.delay(250);
        robot.mouseRelease(mouseLabel);
        robot.delay(250);
        robot.mouseMove(checkedPixelX - 20, checkedPixelY);
        robot.delay(250);
        robot.mouseMove(checkedPixelX + 20, checkedPixelY);
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

    // check if all actions were performed
    harness.check(isMouseDraggedLabel1Flag());
    harness.check(isMouseDraggedLabel2Flag());
    harness.check(isMouseDraggedLabel3Flag());

    // negative tests
    harness.check(!isMouseMovedLabel1Flag());
    harness.check(!isMouseMovedLabel2Flag());
    harness.check(!isMouseMovedLabel3Flag());
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
   * @param mouseDraggedLabel1Flag the mouseDraggedLabel1Flag to set
   */
  public void setMouseDraggedLabel1Flag(boolean mouseDraggedLabel1Flag) {
    this.mouseDraggedLabel1Flag = mouseDraggedLabel1Flag;
  }

  /**
   * @return the mouseDraggedLabel1Flag
   */
  public boolean isMouseDraggedLabel1Flag() {
    return this.mouseDraggedLabel1Flag;
  }

  /**
   * @param mouseDraggedLabel2Flag the mouseDraggedLabel2Flag to set
   */
  public void setMouseDraggedLabel2Flag(boolean mouseDraggedLabel2Flag) {
    this.mouseDraggedLabel2Flag = mouseDraggedLabel2Flag;
  }

  /**
   * @return the mouseDraggedLabel2Flag
   */
  public boolean isMouseDraggedLabel2Flag() {
    return this.mouseDraggedLabel2Flag;
  }

  /**
   * @param mouseDraggedLabel3Flag the mouseDraggedLabel3Flag to set
   */
  public void setMouseDraggedLabel3Flag(boolean mouseDraggedLabel3Flag) {
    this.mouseDraggedLabel3Flag = mouseDraggedLabel3Flag;
  }

  /**
   * @return the mouseDraggedLabel3Flag
   */
  public boolean isMouseDraggedLabel3Flag() {
    return this.mouseDraggedLabel3Flag;
  }

  /**
   * @param mouseMovedLabel1Flag the mouseMovedLabel1Flag to set
   */
  public void setMouseMovedLabel1Flag(boolean mouseMovedLabel1Flag) {
    this.mouseMovedLabel1Flag = mouseMovedLabel1Flag;
  }

  /**
   * @return the mouseMovedLabel1Flag
   */
  public boolean isMouseMovedLabel1Flag() {
    return this.mouseMovedLabel1Flag;
  }

  /**
   * @param mouseMovedLabel2Flag the mouseMovedLabel2Flag to set
   */
  public void setMouseMovedLabel2Flag(boolean mouseMovedLabel2Flag) {
    this.mouseMovedLabel2Flag = mouseMovedLabel2Flag;
  }

  /**
   * @return the mouseMovedLabel2Flag
   */
  public boolean isMouseMovedLabel2Flag() {
    return this.mouseMovedLabel2Flag;
  }

  /**
   * @param mouseMovedLabel3Flag the mouseMovedLabel3Flag to set
   */
  public void setMouseMovedLabel3Flag(boolean mouseMovedLabel3Flag) {
    this.mouseMovedLabel3Flag = mouseMovedLabel3Flag;
  }

  /**
   * @return the mouseMovedLabel3Flag
   */
  public boolean isMouseMovedLabel3Flag() {
    return this.mouseMovedLabel3Flag;
  }
}

