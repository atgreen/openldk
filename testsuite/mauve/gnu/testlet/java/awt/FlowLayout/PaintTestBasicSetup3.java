// Tags: JDK1.4

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

// Tags: GUI, JDK 1.0
// Uses: ../LocationTests

package gnu.testlet.java.awt.FlowLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.java.awt.LocationTests;

import java.awt.Button;
import java.awt.Canvas;
import java.awt.Color;
import java.awt.Component;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Panel;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.FlowLayout;

/**
  * Test if button and canvas are positioned correctly to a frame using FlowLayout.
  */
public class PaintTestBasicSetup3
    extends Panel
    implements Testlet
{

  /**
    * Delay (pause times) for an AWT robot.
    */
  public static int DELAY_AMOUNT=250;

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    this.setLayout(new FlowLayout());
    setBackground(Color.red);
    Frame frame = new Frame();

    // create two components and add them to the panel
    Button button = new Button("             ");
    Canvas canvas = new Canvas();
    canvas.setSize(100,100);
    button.setBackground(Color.blue);
    canvas.setBackground(Color.yellow);
    add(button);
    add(canvas);
    frame.add(this);
    frame.pack();
    frame.show();

    // AWT robot is used for reading pixel colors
    // from a screen and also to wait for all
    // widgets to stabilize theirs size and position.
    Robot robot = harness.createRobot();

    // we should wait a moment before the computations
    // and pixel checks
    robot.waitForIdle();
    robot.delay(DELAY_AMOUNT);

    // check if the first component is properly shown on the panel
    harness.checkPoint("first component");
    harness.check(getColorForComponent(robot, frame, button), Color.blue);

    // check if the second component is properly shown on the panel
    harness.checkPoint("second component");
    harness.check(getColorForComponent(robot, frame, canvas), Color.yellow);

    // There is a delay to avoid any race conditions    
    // and so user can see frame
    robot.waitForIdle();
    robot.delay(DELAY_AMOUNT);

    // it's necesarry to clean up the component from desktop
    frame.dispose();
  }

  /**
    * Get color of a pixel located in the middle of the component.
    *
    * @param robot instance of AWT robot
    * @param frame frame where component is used
    * @param component tested component
    */
  private Color getColorForComponent(Robot robot, Frame frame, Component component)
  {
    // compute the absolute coordinates of a component inside the frame
    Rectangle bounds = computeBounds(frame, component);

    // position of checked pixel
    int checkedPixelX = bounds.x + bounds.width / 2;
    int checkedPixelY = bounds.y + bounds.height / 2;

    // move mouse cursor to center of a rectangle
    moveCursorToGivenPosition(robot, checkedPixelX, checkedPixelY);

    // check the color of a pixel located in the button center
    return robot.getPixelColor(checkedPixelX, checkedPixelY);
  }

  /**
    * Compute absolute coordinates of a component inside the frame
    *
    * @param frame frame where component is used
    * @param component tested component
    *
    * @return coordinates of a component inside the frame
    */
  private Rectangle computeBounds(Frame frame, Component component)
  {
    Rectangle bounds = component.getBounds();
    Point location = frame.getLocationOnScreen();
    Insets insets = frame.getInsets();
    bounds.x += insets.left + location.x;
    bounds.y += insets.top + location.y;

    // return computed bounds
    return bounds;
  }



  /**
    * Move mouse cursor to center of a rectangle
    *
    * @param robot instance of AWT robot
    * @param checkedPixelX x-coordinate of pixel on a screen
    * @param checkedPixelY y-coordinate of pixel on a screen
    */
  private void moveCursorToGivenPosition(Robot robot, int checkedPixelX, int checkedPixelY)
  {
    // move the mouse cursor to a tested pixel to show users what's checked
    robot.delay(DELAY_AMOUNT);
    robot.mouseMove(checkedPixelX, checkedPixelY);
    robot.delay(DELAY_AMOUNT);
    robot.waitForIdle();
  }



  /**
    * Paint method for a panel where components are tested.
    *
    * @param g the graphics context to use for painting
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

