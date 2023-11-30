// Tags: JDK1.5

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

package gnu.testlet.java.awt.GridLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

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
import java.awt.GridLayout;

/**
  * Test if five canvases are positioned correctly to a frame using GridLayout.
  * GridLayout object is constructed using bigger vertical gaps
  * between components.
  *
  * Frame has following layout:
  * <pre>
  * +----------------+----------------+----------------+
  * |                |                |                |
  * |    Canvas 1    |    Canvas 2    |    Canvas 3    |
  * |                |                |                |
  * +----------------+----------------+----------------+
  * |     Space            Space            Space      |
  * +----------------+----------------+                |
  * |                |                |   background   |
  * |    Canvas 4    |    Canvas 5    |     color      |
  * |                |                |                |
  * +----------------+----------------+----------------+
  * </pre>
  *
  * Position of five canvases is tested by checking pixel colors in places
  * marked by a star:
  * <pre>
  * +----------------+----------------+----------------+
  * |                |                |                |
  * |       *        *       *        *       *        |
  * |                |                |                |
  * +----------------+----------------+----------------+
  * |       *        *       *        *                |
  * +----------------+----------------+                |
  * |                |                |                |
  * |       *        *       *        |       *        |
  * |                |                |                |
  * +----------------+----------------+----------------+
  * </pre>
  */
public class PaintTestBiggerVgap
    extends Panel
    implements Testlet
{
  /**
   * Serial version UID because Panel is serializable.
   */
  private static final long serialVersionUID = 42L;

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
    GridLayout layout = new GridLayout(2, 3);
    layout.setVgap(50);

    this.setLayout(layout);
    setBackground(Color.red);
    Frame frame = new Frame();

    // create five components and add them to the panel
    Canvas canvas1 = new Canvas();
    Canvas canvas2 = new Canvas();
    Canvas canvas3 = new Canvas();
    Canvas canvas4 = new Canvas();
    Canvas canvas5 = new Canvas();

    // size of all five canvases
    canvas1.setSize(100,100);
    canvas2.setSize(100,100);
    canvas3.setSize(100,100);
    canvas4.setSize(100,100);
    canvas5.setSize(100,100);

    // background of canvases
    canvas1.setBackground(Color.blue);
    canvas2.setBackground(Color.yellow);
    canvas3.setBackground(Color.magenta);
    canvas4.setBackground(Color.yellow);
    canvas5.setBackground(Color.blue);

    // canvases on frame
    add(canvas1);
    add(canvas2);
    add(canvas3);
    add(canvas4);
    add(canvas5);

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
    robot.delay(DELAY_AMOUNT);

    // check if the first component is properly shown on the panel
    harness.checkPoint("first component");
    harness.check(getColorForComponent(robot, frame, canvas1), Color.blue);

    // check if the second component is properly shown on the panel
    harness.checkPoint("second component");
    harness.check(getColorForComponent(robot, frame, canvas2), Color.yellow);

    // check if the third component is properly shown on the panel
    harness.checkPoint("third component");
    harness.check(getColorForComponent(robot, frame, canvas3), Color.magenta);

    // check if the fourth component is properly shown on the panel
    harness.checkPoint("fourth component");
    harness.check(getColorForComponent(robot, frame, canvas4), Color.yellow);

    // check if the fifth component is properly shown on the panel
    harness.checkPoint("fifth component");
    harness.check(getColorForComponent(robot, frame, canvas5), Color.blue);

    // check background color
    harness.checkPoint("background");
    harness.check(getBackgroundColor(robot, frame, canvas3, canvas5), Color.red);

    // horizontal spaces between components
    // these are negative tests - background should not be visible between components
    harness.checkPoint("space check #1");
    harness.check(getColorBetweenComponents(robot, frame, canvas1, canvas2) != Color.red);
    harness.checkPoint("space check #2");
    harness.check(getColorBetweenComponents(robot, frame, canvas2, canvas3) != Color.red);
    harness.checkPoint("space check #3");
    harness.check(getColorBetweenComponents(robot, frame, canvas4, canvas5) != Color.red);

    // vertical spaces between components
    // these are positive tests - background should be visible between components
    harness.checkPoint("space check #4");
    harness.check(getColorBetweenComponents(robot, frame, canvas1, canvas4), Color.red);
    harness.checkPoint("space check #5");
    harness.check(getColorBetweenComponents(robot, frame, canvas2, canvas5), Color.red);

    // other spaces between components
    // these are positive tests - background should be visible between components
    harness.checkPoint("space check #6");
    harness.check(getColorBetweenComponents(robot, frame, canvas1, canvas5), Color.red);
    harness.checkPoint("space check #7");
    harness.check(getColorBetweenComponents(robot, frame, canvas3, canvas5), Color.red);

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

    // check the color of a pixel located in the canvas center
    return robot.getPixelColor(checkedPixelX, checkedPixelY);
  }



  /**
    * Get background color. Background is located under the third component.
    *
    * @param robot instance of AWT robot
    * @param frame frame where component is used
    * @param component tested component
    */
  private Color getBackgroundColor(Robot robot, Frame frame, Component component1, Component component2)
  {
    // compute the absolute coordinates of a component inside the frame
    Rectangle bounds1 = computeBounds(frame, component1);
    Rectangle bounds2 = computeBounds(frame, component2);

    // position of checked pixel
    int checkedPixelX = bounds1.x + bounds1.width / 2;
    int checkedPixelY = bounds2.y + bounds2.height / 2;

    // move mouse cursor to center of a rectangle
    moveCursorToGivenPosition(robot, checkedPixelX, checkedPixelY);

    // check the color of a pixel located in the canvas center
    return robot.getPixelColor(checkedPixelX, checkedPixelY);
  }



  /**
    * Get color of a pixel located between two components.
    *
    * @param robot instance of AWT robot
    * @param frame frame where component is used
    * @param component1 first tested component
    * @param component2 second tested component
    */
  private Color getColorBetweenComponents(Robot robot, Frame frame, Component component1, Component component2)
  {
    // compute center of the first component
    Point p1 = computeCenterOfComponent(frame, component1);
    // compute center of the second component
    Point p2 = computeCenterOfComponent(frame, component2);

    // compute position of checked pixel
    int checkedPixelX = (p1.x + p2.x) >> 1;
    int checkedPixelY = (p1.y + p2.y) >> 1;

    // move mouse cursor to center of a rectangle
    moveCursorToGivenPosition(robot, checkedPixelX, checkedPixelY);

    // check the color of a pixel located in the canvas center
    return robot.getPixelColor(checkedPixelX, checkedPixelY);
  }



  /**
    * Compute coordinates of a pixel located in the middle of the component.
    *
    * @param frame frame where component is used
    * @param component tested component
    */
  private Point computeCenterOfComponent(Frame frame, Component component)
  {
    // compute the absolute coordinates of a component inside the frame
    Rectangle bounds = computeBounds(frame, component);

    // position of checked pixel
    int checkedPixelX = bounds.x + bounds.width / 2;
    int checkedPixelY = bounds.y + bounds.height / 2;

    // return computed center (position of a checked pixel)
    return new Point(checkedPixelX, checkedPixelY);
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
    * @param graphics the graphics context to use for painting
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

}

