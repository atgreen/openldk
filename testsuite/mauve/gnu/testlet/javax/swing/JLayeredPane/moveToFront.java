/* moveToFront.java -- Checks the moveToFront method in JLayeredPane
   Copyright (C) 2006 Roman Kennke (kennke@aicas.com)
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.JLayeredPane;

import javax.swing.JLayeredPane;
import javax.swing.JPanel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks the moveToFront method in JLayeredPane.
 * 
 * @author Roman Kennke (kennke@aicas.com)
 */
public class moveToFront implements Testlet
{

  /** 
   * Starts the test.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testSimple(harness);
  }

  /**
   * A rather simple test for this method.
   *
   * @param harness the test harness to use
   */
  private void testSimple(TestHarness harness)
  {
    harness.checkPoint("testSimple");
    JLayeredPane l = new JLayeredPane();
    JPanel p1 = new JPanel();
    JPanel p2 = new JPanel();
    JPanel p3 = new JPanel();
    l.add(p1);
    l.add(p2);
    l.add(p3);
    l.moveToFront(p2);
    harness.check(l.getComponent(0), p2);
    harness.check(l.getComponent(1), p1);
    harness.check(l.getComponent(2), p3);
  }
}
