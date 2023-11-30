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

package gnu.testlet.java.awt.Window;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.*;
import java.awt.event.*;

/**
 * Check that Window.pack gives children the right sizes.
 */
public class pack1 implements Testlet
{
  Color bg = new Color (243, 133, 89);
  Color border = new Color (255, 0, 0);

  class pack1Frame extends Frame
  {
    public pack1Frame ()
    {
      super ();
    }
  }

  class pack1Canvas extends Canvas
  {
    public pack1Canvas ()
    {
      super ();
      setBackground (bg);
    }

    public void paint (Graphics g)
    {
      Rectangle bounds = new Rectangle (0, 0,
					this.getSize ().width,
					this.getSize ().height);
      g.setColor (border);
      g.drawRect (bounds.x, bounds.y, bounds.width - 1, bounds.height - 1);
    }
  }

  public void checkColor (TestHarness harness, Color c)
  {
    harness.check (c.getRed () == border.getRed ()
		   && c.getGreen () == border.getGreen ()
		   && c.getBlue () == border.getBlue ());
  }

  public void test (TestHarness harness)
  {
    Robot r = harness.createRobot ();
    Color c;
    int x = 100;
    int y = 100;
    int width = 100;
    int height = 100;
    Insets i;

    pack1Frame f = new pack1Frame ();

    f.setLocation (x, y);
    f.setSize (width, height);

    f.add (new pack1Canvas ());

    r.setAutoDelay (100);
    r.setAutoWaitForIdle (true);

    f.show ();

    r.waitForIdle ();
    r.delay (100);

    i = f.getInsets ();

    int top_y = y + i.top;
    int bottom_y = y + height - i.bottom - 1;
    int left_x = x + i.left;
    int right_x = x + width - i.right - 1;

    // top-left-left
    c = r.getPixelColor (left_x, top_y + 1);
    checkColor (harness, c);

    // top-left-top
    c = r.getPixelColor (left_x + 1, top_y);
    checkColor (harness, c);

    // top-right-right
    c = r.getPixelColor (right_x, top_y + 1);
    checkColor (harness, c);

    // top-right-top
    c = r.getPixelColor (right_x - 1, top_y);
    checkColor (harness, c);

    // bottom-left-left
    c = r.getPixelColor (left_x, bottom_y - 1);
    checkColor (harness, c);

    // bottom-left-bottom
    c = r.getPixelColor (left_x + 1, bottom_y);
    checkColor (harness, c);

    // bottom-right-right
    c = r.getPixelColor (right_x, bottom_y - 1);
    checkColor (harness, c);

    // bottom-right-bottom
    c = r.getPixelColor (right_x - 1, bottom_y);
    checkColor (harness, c);

    r.delay (4000);
  }
}
