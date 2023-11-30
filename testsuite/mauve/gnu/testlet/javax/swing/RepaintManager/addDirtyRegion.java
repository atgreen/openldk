// Tags: JDK1.2
// Uses: DisabledEventQueue

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.RepaintManager;

import java.awt.Rectangle;
import java.awt.Toolkit;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.RepaintManager;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks the addDirtyRegion method. More specifically, this checks if
 * addDirtyRegion does any optimization or not.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class addDirtyRegion implements Testlet
{

  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    test1(harness);
  }

  /**
   * Adds a dirty region for a component and checks if it is correctly
   * returned by getDirtyRegion() and isCompletelyDirty().
   *
   * @param harness the test harness to use
   */
  private void test1(TestHarness harness)
  {
    // Disable event queue to prevent the RepaintManager from working.
    Toolkit.getDefaultToolkit().getSystemEventQueue()
    .push(new DisabledEventQueue());

    JFrame f = new JFrame();
    JLabel l = new JLabel("Hello");
    f.getContentPane().add(l);
    l.setSize(100, 100);
    f.setSize(200, 200);
    f.setVisible(true);
    RepaintManager rm = RepaintManager.currentManager(l);
    rm.addDirtyRegion(l, 0, 0, l.getWidth(), l.getHeight());
    harness.check(rm.isCompletelyDirty(l), false);
    Rectangle dirty = rm.getDirtyRegion(l);
    harness.check(dirty.x, 0);
    harness.check(dirty.y, 0);
    harness.check(dirty.width, l.getWidth());
    harness.check(dirty.height, l.getHeight());
  }
}
