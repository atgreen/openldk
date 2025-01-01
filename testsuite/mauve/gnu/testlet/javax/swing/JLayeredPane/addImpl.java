/* addImpl.java -- Tests if addImpl in JLayeredPane works correctly
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
 * Tests if addImpl in JLayeredPane works correctly.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class addImpl implements Testlet
{

  /**
   * The entry point of this test.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testAddSameLayer(harness);
    testAddDifferentLayers(harness);
    testAddZeroPosition(harness);
  }

  /**
   * Tests the addition of components into the same layer with no position
   * specified.
   *
   * @param h the test harness to use
   */
  private void testAddSameLayer(TestHarness h)
  {
    JLayeredPane l = new JLayeredPane();
    JPanel p1 = new JPanel();
    JPanel p2 = new JPanel();
    JPanel p3 = new JPanel();
    l.add(p1, JLayeredPane.FRAME_CONTENT_LAYER);
    l.add(p2, JLayeredPane.FRAME_CONTENT_LAYER);
    l.add(p3, JLayeredPane.FRAME_CONTENT_LAYER);
    h.check(l.getComponent(0), p1);
    h.check(l.getComponent(1), p2);
    h.check(l.getComponent(2), p3);
  }

  /**
   * Tests the addition of 3 components into different layers with -1
   * positions.
   *
   * @param h the test harness to use
   */
  private void testAddDifferentLayers(TestHarness h)
  {
    JLayeredPane l = new JLayeredPane();
    JPanel p1 = new JPanel();
    JPanel p2 = new JPanel();
    JPanel p3 = new JPanel();
    l.add(p1, new Integer(0));
    l.add(p2, new Integer(1));
    l.add(p3, new Integer(2));
    h.check(l.getComponent(0), p3);
    h.check(l.getComponent(1), p2);
    h.check(l.getComponent(2), p1);
  }

  /**
   * Tests the addition of 3 components into the same layer and with a position
   * argument of 0. This should add each component as the top component in
   * that layer.
   *
   * @param h the test harness to use
   */
  private void testAddZeroPosition(TestHarness h)
  {
    JLayeredPane l = new JLayeredPane();
    JPanel p1 = new JPanel();
    JPanel p2 = new JPanel();
    JPanel p3 = new JPanel();
    l.add(p1, JLayeredPane.FRAME_CONTENT_LAYER, 0);
    l.add(p2, JLayeredPane.FRAME_CONTENT_LAYER, 0);
    l.add(p3, JLayeredPane.FRAME_CONTENT_LAYER, 0);
    h.check(l.getComponent(0), p3);
    h.check(l.getComponent(1), p2);
    h.check(l.getComponent(2), p1);
  }
}
