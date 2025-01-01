// Tags: GUI JDK1.0

// Copyright (C) 2005 Red Hat

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

package gnu.testlet.java.awt.Component;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.*;
import java.awt.event.*;

/**
 * Check that a click events contain the proper modifier information.
 */
public class clickModifiers implements Testlet
{
  int modifiers = 0;

  class clickModifiersFrame extends Frame
  {
    clickModifiersFrame ()
    {
      super ();
    }
  }

  public void test (TestHarness harness)
  {
    Robot r = harness.createRobot ();
    int x = 100;
    int y = 100;
    int width = 100;
    int height = 100;

    clickModifiersFrame f = new clickModifiersFrame ();

    f.setLocation (x, y);
    f.setSize (width, height);

    f.addMouseListener (new MouseAdapter ()
      {
	public void mouseClicked (MouseEvent e)
	{
	  modifiers = e.getModifiers ();
	}
      });

    r.setAutoDelay (100);
    r.setAutoWaitForIdle (true);

    f.show ();

    r.mouseMove (x + width / 2, y + height / 2);

    // left click
    r.mousePress (InputEvent.BUTTON1_MASK);
    r.mouseRelease (InputEvent.BUTTON1_MASK);

    harness.check (modifiers == InputEvent.BUTTON1_MASK);

    // right click
    r.mousePress (InputEvent.BUTTON2_MASK);
    r.mouseRelease (InputEvent.BUTTON2_MASK);

    harness.check (modifiers == InputEvent.BUTTON2_MASK);

    // middle click
    r.mousePress (InputEvent.BUTTON3_MASK);
    r.mouseRelease (InputEvent.BUTTON3_MASK);

    harness.check (modifiers == InputEvent.BUTTON3_MASK);
  }
}
