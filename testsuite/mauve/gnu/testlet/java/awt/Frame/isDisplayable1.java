// Tags: GUI JDK1.0

// Copyright (C) 2005, 2011 Red Hat

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

package gnu.testlet.java.awt.Frame;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.*;

/**
 * Tests the behaviour of isDisplayable on Frames and Dialogs.  Also
 * check whether a Dialog's parent's "displayability" affects its
 * displayability.
 */
public class isDisplayable1 implements Testlet
{
  public void test (TestHarness harness)
  {
    Frame f = new Frame ();
    Dialog d = new Dialog (f);
    Robot r = harness.createRobot ();

    r.waitForIdle ();

    // Check that the owner frame is set as the Dialog's parent.
    harness.checkPoint ("parent check");
    harness.check (d.getParent(), f);

    harness.checkPoint ("before showing");
    harness.check (f.getPeer (), null);
    harness.check (d.getPeer (), null);
    harness.check (f.isDisplayable (), false);
    harness.check (d.isDisplayable (), false);

    f.show ();

    r.waitForIdle ();

    // Parent Frame is displayable, child Dialog is not.
    harness.checkPoint ("after showing frame");
    harness.check (f.getPeer () != null);
    harness.check (d.getPeer (), null);
    harness.check (f.isDisplayable (), true);
    harness.check (d.isDisplayable (), false);

    d.show ();

    r.waitForIdle ();

    // Dialog becomes displayable after it is shown.
    harness.checkPoint ("after showing dialog");
    harness.check (f.getPeer () != null);
    harness.check (d.getPeer () != null);
    harness.check (f.isDisplayable (), true);
    harness.check (d.isDisplayable (), true);

    // time to clean up the frame from desktop
    f.dispose();
  }
}
