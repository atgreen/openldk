// KeyboardListenerAsciiKeys.java -- 

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

package gnu.testlet.java.awt.Checkbox;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.java.awt.LocationTests;

import java.awt.Checkbox;
import java.awt.Color;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Panel;
import java.awt.Point;
import java.awt.Robot;
import java.awt.Rectangle;
import java.awt.event.*;
import java.util.*;

/**
  * Check if KeyListener could be registered for an AWT Checkbox
  * and if action is really performed.
  */
public class KeyboardListenerAsciiKeys
    extends Panel
    implements Testlet
{

  int[] keysToTest = {
      KeyEvent.VK_A,
      KeyEvent.VK_Z,
      KeyEvent.VK_0,
      KeyEvent.VK_9,
  };

  // these flags are set by KeyListener
  List<Integer> keyPressedFlag = new ArrayList<Integer>();
  List<Integer> keyReleasedFlag = new ArrayList<Integer>();
  List<Integer> keyTypedFlag = new ArrayList<Integer>();

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    setBackground(Color.red);
    Frame frame = new Frame();
    Checkbox checkbox = new Checkbox("xyzzy");
    checkbox.setBackground(Color.blue);
    add(checkbox);

    // register new listener
    checkbox.addKeyListener(
      new KeyListener() {

        public void keyPressed(KeyEvent e) 
        {
          keyPressedFlag.add(e.getKeyCode());
        }

        public void keyReleased(KeyEvent e) 
        {
          keyReleasedFlag.add(e.getKeyCode());
        }

        public void keyTyped(KeyEvent e) 
        {
          keyTypedFlag.add(0+e.getKeyChar());
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

    // compute absolute coordinations of checkbox on a screen
    Rectangle bounds = checkbox.getBounds();
    Point loc = frame.getLocationOnScreen();
    Insets insets = frame.getInsets();
    bounds.x += insets.left + loc.x;
    bounds.y += insets.top + loc.y;

    // position of checked pixel
    int checkedPixelX = bounds.x + bounds.width / 2;
    int checkedPixelY = bounds.y + bounds.height / 2;

    robot.mouseMove(checkedPixelX, checkedPixelY);
    robot.waitForIdle();
    robot.delay(250);

    // have to use 1.4 syntax
    for (int i = 0; i < keysToTest.length; i++)
    {
        int key = keysToTest[i];
        robot.keyPress(key);
        robot.delay(250);
        robot.keyRelease(key);
        robot.delay(250);
    }

    // There is a delay to avoid any race conditions    
    // and so user can see frame
    robot.waitForIdle();
    robot.delay(1000);

    // it's necesarry to clean up the component from desktop
    frame.dispose();

    // check if all actions were correctly performed
    for (int i = 0; i < keysToTest.length; i++)
    {
      int key = keysToTest[i];
      harness.check(keyPressedFlag.contains(key));
    }
    for (int i = 0; i < keysToTest.length; i++)
    {
      int key = keysToTest[i];
      harness.check(keyReleasedFlag.contains(key));
    }
    for (int i = 0; i < keysToTest.length; i++)
    {
      int key = keysToTest[i];
      // either 'A' or 'a' could be catched by keyTyped event
      harness.check(keyTypedFlag.contains(key) || keyTypedFlag.contains(key - 'A' + 'a'));
    }
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

