// ItemListenerPositiveTest.java -- 

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

import java.awt.*;
import java.awt.event.*;

/**
  * Check if ItemListener could be registered for an AWT Checkbox
  * and if action is performed when left mouse checkbox is pressed.
  */
public class ItemListenerPositiveTest
    extends Panel
    implements Testlet
{
  boolean itemStateChangedFlag = false;

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
    checkbox.addItemListener(
      new ItemListener() {
        public void itemStateChanged(ItemEvent e)
        {
          itemStateChangedFlag = true;
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

    Rectangle bounds = checkbox.getBounds();
    Point loc = frame.getLocationOnScreen();
    Insets i = frame.getInsets();
    bounds.x += i.left + loc.x;
    bounds.y += i.top + loc.y;

    // position of checked pixel
    int checkedPixelX = bounds.x + bounds.width / 2;
    int checkedPixelY = bounds.y + bounds.height / 2;

    // move the mouse cursor to a tested pixel to show users what's checked
    robot.mouseMove(checkedPixelX, checkedPixelY);
    robot.waitForIdle();
    robot.mousePress(InputEvent.BUTTON1_MASK);
    robot.delay(250);
    robot.mouseRelease(InputEvent.BUTTON1_MASK);
    robot.delay(250);

    // There is a delay to avoid any race conditions    
    // and so user can see frame
    robot.waitForIdle();
    robot.delay(1000);

    // it's necesarry to clean up the component from desktop
    frame.dispose();

    // check if action was performed
    harness.check(itemStateChangedFlag);
  }

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

