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

package gnu.testlet.java.awt.CardLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.java.awt.LocationTests;

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
import java.awt.CardLayout;



/**
  * Test if five canvases are positioned correctly to a frame using CardLayout.
  * Zero-width gaps are used during positioning canvases on a frame, so we also have to
  * test if the gaps are not visible at all.
  */
public class PaintTestZeroGaps
    extends Panel
    implements Testlet
{

  /**
    * Delay (pause times) for an AWT robot.
    */
  public static int DELAY_AMOUNT = 250;

  /**
    * Horizontal gap size.
    */
  public static int HGAP_SIZE = 0;
  
  /**
    * Vertical gap size.
    */
  public static int VGAP_SIZE = 0;

  /**
    * Offset (in pixels) from the "true" border
    * to check hgaps and vgaps visibility
    * This value should be greater than zero and less than *GAP_SIZE.
    */
  public static int OFFSET_FROM_BORDER = 10;

  /**
    * Default background color for a panel.
    */
  public static Color BACKGROUND_COLOR = Color.red;

  /**
    * Runs the test using the specified harness. 
    * 
    * @param harness  the test harness (<code>null</code> not permitted).
    */
  public void test(TestHarness harness)
  {
    CardLayout layout = new CardLayout(HGAP_SIZE, VGAP_SIZE);
    this.setLayout(layout);
    setBackground(BACKGROUND_COLOR);
    Frame frame = new Frame();

    // create five components and add them to the panel
    Canvas canvas1 = new Canvas();
    Canvas canvas2 = new Canvas();
    Canvas canvas3 = new Canvas();
    Canvas canvas4 = new Canvas();
    Canvas canvas5 = new Canvas();

    // set size of all canvases
    canvas1.setSize(100,100);
    canvas2.setSize(100,100);
    canvas3.setSize(100,100);
    canvas4.setSize(100,100);
    canvas5.setSize(100,100);

    // set background of all canvases
    canvas1.setBackground(Color.blue);
    canvas2.setBackground(Color.yellow);
    canvas3.setBackground(Color.magenta);
    canvas4.setBackground(Color.yellow);
    canvas5.setBackground(Color.cyan);

    // position all five canvases on frame
    add(canvas1);
    add(canvas2);
    add(canvas3);
    add(canvas4);
    add(canvas5);

    // setup for a frame
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
    layout.first(this);
    doTest(harness, robot, frame, canvas1, Color.blue);

    // check if the second component is properly shown on the panel
    harness.checkPoint("second component");
    layout.next(this);
    doTest(harness, robot, frame, canvas2, Color.yellow);

    // check if the third component is properly shown on the panel
    harness.checkPoint("third component");
    layout.next(this);
    doTest(harness, robot, frame, canvas3, Color.magenta);

    // check if the fourth component is properly shown on the panel
    harness.checkPoint("fourth component");
    layout.next(this);
    doTest(harness, robot, frame, canvas4, Color.yellow);

    // check if the fifth component is properly shown on the panel
    harness.checkPoint("fifth component");
    layout.next(this);
    doTest(harness, robot, frame, canvas5, Color.cyan);

    // There is a delay to avoid any race conditions
    // and so user can see frame
    robot.waitForIdle();
    robot.delay(DELAY_AMOUNT);

    // it's necesarry to clean up the component from desktop
    frame.dispose();
  }



  /**
   * Runs the test for one component using the specified harness. 
   * 
   * @param harness        the test harness (<code>null</code> not permitted).
   * @param robot          instance of AWT robot
   * @param frame          frame where component is used
   * @param component      tested component
   * @param componentColor background color of tested component
   */
  private void doTest(TestHarness harness, Robot robot, Frame frame, Component component, Color componentColor)
  {
    // There is a delay to avoid any race conditions
    // and so user can see frame
    robot.waitForIdle();
    robot.delay(DELAY_AMOUNT);

    // check color inside the component
    harness.check(getColorForComponent(robot, frame, component), componentColor);
    // check color on the borders - there should not be any borders visible
    harness.check(getColorForLeftBorder(robot, frame, component),  componentColor);
    harness.check(getColorForRightBorder(robot, frame, component), componentColor);
    harness.check(getColorForTopBorder(robot, frame, component), componentColor);
    harness.check(getColorForBottomBorder(robot, frame, component), componentColor);
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

    // compute the position of a center of a component
    Point p = computeCenterOfComponent(frame, component);

    // move mouse cursor to center of a rectangle
    moveCursorToGivenPosition(robot, p.x, p.y);

    // check the color of a pixel located in the canvas center
    return robot.getPixelColor(p.x, p.y);
  }



  /**
    * Get color of a pixel located in the left border.
    *
    * @param robot instance of AWT robot
    * @param frame frame where component is used
    * @param component tested component
    */
  private Color getColorForLeftBorder(Robot robot, Frame frame, Component component)
  {

    // compute the position of a center of a component
    Point p = computeCenterOfComponent(frame, component);
    p.x = OFFSET_FROM_BORDER;

    moveCursorToGivenPosition(robot, p.x, p.y);

    // check the color of a pixel located in the canvas center
    return robot.getPixelColor(p.x, p.y);
  }



  /**
    * Get color of a pixel located in the right border.
    *
    * @param robot instance of AWT robot
    * @param frame frame where component is used
    * @param component tested component
    */
  private Color getColorForRightBorder(Robot robot, Frame frame, Component component)
  {

    // compute the position of a center of a component
    Point p = computeCenterOfComponent(frame, component);
    p.x = frame.getWidth() - OFFSET_FROM_BORDER;

    moveCursorToGivenPosition(robot, p.x, p.y);

    // check the color of a pixel located in the canvas center
    return robot.getPixelColor(p.x, p.y);
  }



  /**
    * Get color of a pixel located in the top border.
    *
    * @param robot instance of AWT robot
    * @param frame frame where component is used
    * @param component tested component
    */
  private Color getColorForTopBorder(Robot robot, Frame frame, Component component)
  {

    // compute the position of a center of a component
    Point p = computeCenterOfComponent(frame, component);
    p.y = p.y - component.getHeight()/2 + OFFSET_FROM_BORDER;

    moveCursorToGivenPosition(robot, p.x, p.y);

    // check the color of a pixel located in the canvas center
    return robot.getPixelColor(p.x, p.y);
  }



  /**
    * Get color of a pixel located in the bottom border.
    *
    * @param robot instance of AWT robot
    * @param frame frame where component is used
    * @param component tested component
    */
  private Color getColorForBottomBorder(Robot robot, Frame frame, Component component)
  {

    // compute the position of a center of a component
    Point p = computeCenterOfComponent(frame, component);
    p.y = p.y + component.getHeight()/2 - OFFSET_FROM_BORDER;

    moveCursorToGivenPosition(robot, p.x, p.y);

    // check the color of a pixel located in the canvas center
    return robot.getPixelColor(p.x, p.y);
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
    * Move mouse cursor to center of a rectangle
    *
    * @param robot instance of AWT robot
    * @param checkedPixelX x-coordinate of pixel on a screen
    * @param checkedPixelY y-coordinate of pixel on a screen
    */
  private void moveCursorToGivenPosition(Robot robot, int checkedPixelX, int checkedPixelY)
  {
    // move the mouse cursor to a tested pixel to visually show users what's checked
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

