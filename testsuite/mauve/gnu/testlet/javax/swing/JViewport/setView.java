// Tags: JDK1.2
// Uses: TestViewport

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

package gnu.testlet.javax.swing.JViewport;

import javax.swing.JComponent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks if setView() calls revalidate and repaint.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class setView implements Testlet
{

  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testRepaint(harness);
    testRevalidate(harness);
  }

  /**
   * Tests if setView triggers a repaint. setView should trigger a
   * repaint() call whenever method is called, independent of
   * actual value change.
   *
   * @param harness the test harness to use
   */
  private void testRepaint(TestHarness harness)
  {
    JComponent c1 = new JComponent(){};
    JComponent c2 = new JComponent(){};
    TestViewport c = new TestViewport();
    // Set to null, so that we know the state.
    c.setView(null);
    c.repaintCalled = false;
    // Change state and check if repaint is called.
    c.setView(c1);
    harness.check(c.repaintCalled, true);
    // Don't change state.
    c.repaintCalled = false;
    c.setView(c1);
    harness.check(c.repaintCalled, true);
    // Change state and check if repaint is called.
    c.repaintCalled = false;
    c.setView(c2);
    harness.check(c.repaintCalled, true);
    // Don't change state.
    c.repaintCalled = false;
    c.setView(c2);
    harness.check(c.repaintCalled, true);
  }

  /**
   * Tests if setView triggers a revalidate. setView should trigger
   * a revalidate(), independent of actual value change.
   *
   * @param harness the test harness to use
   */
  private void testRevalidate(TestHarness harness)
  {
    JComponent c1 = new JComponent(){};
    JComponent c2 = new JComponent(){};
    TestViewport c = new TestViewport();
    // Set to null, so that we know the state.
    c.setView(null);
    c.revalidateCalled = false;
    // Change state and check if repaint is called.
    c.setView(c1);
    harness.check(c.revalidateCalled, true);
    // Don't change state.
    c.revalidateCalled = false;
    c.setView(c1);
    harness.check(c.revalidateCalled, true);
    // Change state and check if repaint is called.
    c.revalidateCalled = false;
    c.setView(c2);
    harness.check(c.revalidateCalled, true);
    // Don't change state.
    c.revalidateCalled = false;
    c.setView(c2);
    harness.check(c.revalidateCalled, true);
  }
}
