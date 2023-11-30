// Tags: JDK1.2 GUI

// Copyright (C) 2004 Audrius Meskauskas <audriusa@bluewin.ch>

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

package gnu.testlet.javax.swing.JFrame;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;

import javax.swing.JComponent;
import javax.swing.JFrame;

/**
 * Tests how the JFrame paints its five children, verifying
 * the supplied boundaries and invocation of paint(Graphics).
 * Needs GUI to run.
 * @author Audrius Meskauskas (AudriusA@BlueWin.ch)>
 */
public class paint5
  implements Testlet
{
  /**
   * Width (and height) of the frame (+caption)
   */
  static final int FRAME_SIZE = 200;

  /**
   * The preferred width and height for all children.
   */
  static final int CHILD_SIZE = FRAME_SIZE / 4;
  component north = new component("north");
  component center = new component("center");
  component south = new component("south");
  component east = new component("east");
  component west = new component("west");
  Dimension pane;
  int totalPainted = 0;
  Thread thread;

  public JFrame showFrame()
  {
    JFrame frame = new JFrame();
    frame.getContentPane().add(north, BorderLayout.NORTH);
    frame.getContentPane().add(south, BorderLayout.SOUTH);
    frame.getContentPane().add(east, BorderLayout.EAST);
    frame.getContentPane().add(west, BorderLayout.WEST);
    frame.getContentPane().add(center, BorderLayout.CENTER);
    frame.setSize(new Dimension(FRAME_SIZE, FRAME_SIZE));
    frame.setVisible(true);

    pane = frame.getContentPane().getSize();
    return frame;
  }

  class component
    extends JComponent
  {
    String name;
    int painted;
    Rectangle bounds;
    Color color;

    component(String _name)
    {
      name = _name;
      setPreferredSize(new Dimension(CHILD_SIZE, CHILD_SIZE));
    }

    public void paint(Graphics g)
    {
      painted++;

      color = g.getColor();
      bounds = getBounds();

      // The purpose of drawing and writing is to ensure that the application
      // does not hang.
      g.setColor(Color.white);
      g.fillOval(0, 0, bounds.width, bounds.height);
      g.setColor(Color.red);
      g.drawString(name, CHILD_SIZE / 2, CHILD_SIZE / 2);

      totalPainted++;

      if (totalPainted >= 5)
        thread.interrupt();
    }

    void basicCheck(TestHarness h)
    {
      h.check(bounds != null, name + " seems never painted");
      h.check(color.getRed() == 0 && color.getGreen() == 0 &&
              color.getBlue() == 0, name + ": default color must be black"
             );
      h.check(painted, 1, name + " must be painted exactly once");

      h.checkPoint(name + " placement");
    }
  }

  public void test(TestHarness harness)
  {
    thread = Thread.currentThread();

    JFrame frame = showFrame();
    try
      {
        // Wait for 10 seconds at most, but should be interrupted much earlier.
        Thread.sleep(10000);
      }
    catch (InterruptedException ex)
      {
      }

    north.basicCheck(harness);
    harness.check(north.bounds.width, pane.width, " north, width");
    harness.check(north.bounds.height, CHILD_SIZE, "north, height");
    harness.check(north.bounds.x, 0, "north, x");
    harness.check(north.bounds.y, 0, "north, y");

    south.basicCheck(harness);
    harness.check(south.bounds.width, pane.width, "south, width");
    harness.check(south.bounds.height, CHILD_SIZE, "south, height");
    harness.check(south.bounds.x, 0, "south, x");
    harness.check(south.bounds.y, pane.height - CHILD_SIZE, "south, y");

    east.basicCheck(harness);
    harness.check(east.bounds.width, CHILD_SIZE, "east, width");
    harness.check(east.bounds.height, pane.height - 2 * CHILD_SIZE,
                  "east, height"
                 );
    harness.check(east.bounds.x, pane.width - CHILD_SIZE, "east, x");
    harness.check(east.bounds.y, CHILD_SIZE, "east, y");

    west.basicCheck(harness);
    harness.check(west.bounds.width, CHILD_SIZE, "west, width");
    harness.check(west.bounds.height, pane.height - 2 * CHILD_SIZE,
                  "west, height"
                 );
    harness.check(west.bounds.x, 0, "west, x");
    harness.check(west.bounds.y, CHILD_SIZE, "west, y");

    center.basicCheck(harness);
    harness.check(center.bounds.width, pane.width - 2 * CHILD_SIZE,
                  "center, width"
                 );
    harness.check(center.bounds.height, pane.height - 2 * CHILD_SIZE,
                  "center, height"
                 );
    harness.check(center.bounds.x, CHILD_SIZE, "center, x");
    harness.check(center.bounds.y, CHILD_SIZE, "center, y");

    try
      {
        Thread.sleep(200);
      }
    catch (InterruptedException ex)
      {
      }

    frame.setVisible(false);
    frame.dispose();
    frame = null;
    thread = null;
    pane = null;
    north = center = south = east = null;
  }
}
