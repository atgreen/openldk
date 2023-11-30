// Tags: JDK1.2
// Uses: TestComponent

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

package gnu.testlet.javax.swing.JComponent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests if setAlignmentY works correctly.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class setAlignmentY implements Testlet
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
   * Tests if setAlignmentY triggers a repaint.
   *
   * @param harness the test harness to use
   */
  private void testRepaint(TestHarness harness)
  {
    TestComponent c = new TestComponent();
    // Set to 0.0, so that we know the state.
    c.setAlignmentY(0.0F);
    c.repaintCalled = false;
    // Change state and check if repaint is called.
    c.setAlignmentY(0.5F);
    harness.check(c.repaintCalled, false);
    // Don't change state.
    c.repaintCalled = false;
    c.setAlignmentY(0.5F);
    harness.check(c.repaintCalled, false);
    // Change state and check if repaint is called.
    c.repaintCalled = false;
    c.setAlignmentY(1.0F);
    harness.check(c.repaintCalled, false);
    // Don't change state.
    c.repaintCalled = false;
    c.setAlignmentY(1.0F);
    harness.check(c.repaintCalled, false);
  }

  /**
   * Tests if setAlignmentY triggers a revalidate.
   *
   * @param harness the test harness to use
   */
  private void testRevalidate(TestHarness harness)
  {
    TestComponent c = new TestComponent();
    // Set to 0.0, so that we know the state.
    c.setAlignmentY(0.0F);
    c.revalidateCalled = false;
    // Change state and check if repaint is called.
    c.setAlignmentY(0.5F);
    harness.check(c.revalidateCalled, false);
    // Don't change state.
    c.revalidateCalled = false;
    c.setAlignmentY(0.5F);
    harness.check(c.revalidateCalled, false);
    // Change state and check if repaint is called.
    c.revalidateCalled = false;
    c.setAlignmentY(1.0F);
    harness.check(c.revalidateCalled, false);
    // Don't change state.
    c.revalidateCalled = false;
    c.setAlignmentY(1.0F);
    harness.check(c.revalidateCalled, false);
  }
}
