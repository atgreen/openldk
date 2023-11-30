// Tags: GUI JDK1.3

// Copyright (C) 2004 Red Hat

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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.awt.Robot;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.*;
import java.awt.event.*;
import java.awt.AWTException;

/**
 * Checks that getPixelColor in the {@link Robot} class can read a
 * pixel's color value from the screen.
 */
public class getPixelColor implements Testlet
{
  /**
   * Runs the test using the specified harness.
   *
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test (TestHarness harness)
  {
    Robot r = null;

    // construct robot
    try
      {
	r = new Robot ();
      }
    catch (AWTException e)
      {
	harness.fail ("caught AWT exception: " + e.getMessage ());
      }

    Frame f = new Frame ();
    Panel p = new Panel ();

    p.setBackground (new Color (255, 0, 0));

    f.add (p);

    f.setSize (100, 100);
    f.setLocation (250, 250);
    f.show ();

    r.waitForIdle ();

    Color c = r.getPixelColor (300, 300);

    r.waitForIdle ();

    // check that pixel is red
    harness.check (c.getRed () == 255
		   && c.getGreen () == 0
		   && c.getBlue () == 0);
  }
}
