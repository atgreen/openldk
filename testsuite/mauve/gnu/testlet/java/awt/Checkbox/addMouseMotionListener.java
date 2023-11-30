// addMouseMotionListener.java -- 

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
  * Check if MouseMotionListener could be registered for an AWT Checkbox.
  */
public class addMouseMotionListener
    implements Testlet
{

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    Checkbox checkbox = new Checkbox("xyzzy");
    checkbox.setBackground(Color.blue);

    // array which will be filled by registered listeners
    MouseMotionListener[] mouseMotionListeners;

    // get all registered listeners
    mouseMotionListeners = checkbox.getMouseMotionListeners();
    harness.check(mouseMotionListeners.length, 0);

    // register new listener
    checkbox.addMouseMotionListener(
      new MouseMotionListener() {

        public void mouseDragged(MouseEvent e)
        {
          // empty
        }

        public void mouseMoved(MouseEvent e)
        {
          // empty
        }

        @Override
        public String toString()
        {
          return "myMouseMotionListener";
        }
      }
    );

    // get all registered listeners
    mouseMotionListeners = checkbox.getMouseMotionListeners();
    harness.check(mouseMotionListeners.length, 1);

    // check if the proper listener is used
    harness.check(mouseMotionListeners[0].toString(), "myMouseMotionListener");
  }
}

