/* getComponentAt.java --
   Copyright (C) 2006 Red Hat
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

package gnu.testlet.java.awt.Container;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Frame;
import java.awt.List;
import java.awt.Panel;
import java.awt.Point;
import java.awt.Robot;
import java.awt.TextArea;

public class getComponentAt implements Testlet
{
  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    Frame f = new Frame();
    List l = new List(10);
    TextArea t = new TextArea(10, 10);
    f.setSize(300, 300);
    t.setBounds(10, 10, 100, 100);
    l.setBounds(0, 0, 100, 100);
    
    t.setVisible(true);
    l.setVisible(false);
    f.add(l);
    f.add(t);
    f.show();
    
    Robot r = harness.createRobot();
    r.waitForIdle();
    r.delay(1000);
    
    Point po = l.getLocation();
    harness.check(t.isVisible(), true);
    harness.check(f.isVisible(), true);
    harness.check(l.isVisible(), false);
    harness.check(f.getComponentAt(po.x, po.y) == l);

    // time to clean up
    f.dispose();
  }
}
