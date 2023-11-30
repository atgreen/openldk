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
import java.awt.image.*;
import java.awt.AWTException;

/**
 * Checks that createScreenCapture in the {@link Robot} class can
 * produce a screenshot.
 */
public class createScreenCapture implements Testlet
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

    f.setSize (500, 500);
    f.setLocation (0, 0);
    f.show ();

    r.waitForIdle ();

    BufferedImage screenshot = r.createScreenCapture (new Rectangle (50, 50, 100, 100));

    // check that captured image is completely red
    boolean passed = true;

    for (int i = 0; i < 100; i++)
      {
	for (int j = 0; j < 100; j++)
	  {
	    Color c = new Color (screenshot.getRGB (i, j));

	    if (c.getRed () != 255 || c.getGreen () != 0 || c.getBlue () != 0)
	      passed = false;
	  }
      }

    harness.check (passed);
  }
}
