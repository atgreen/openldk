//Tags: JDK1.2
//Uses: TestComponent

//Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version. 

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.JComponent;

import javax.swing.JFrame;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
* Tests if setVisible works correctly.
*
* @author Roman Kennke (kennke@aicas.com)
*/
public class setVisible implements Testlet
{
  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testRepaint(harness);
    testRepaintNotShowing(harness);
    testRevalidate(harness);
  }
  
  /**
   * Tests if setVisible triggers a repaint.
   *
   * @param harness the test harness to use
   */
  private void testRepaint(TestHarness harness)
  {
    TestComponent c = new TestComponent();
    JFrame f = new JFrame();
    f.getContentPane().add(c);
    f.setSize(200, 200);
    f.setVisible(true);
    c.setBounds(10, 20, 30, 40);
    // Set to false, so that we know the state.
    c.setVisible(false);
    c.repaintCalled = false;
    // Change state and check if repaint is called.
    c.setVisible(true);
    harness.check(c.repaintCalled, true);
    // Don't change state.
    c.repaintCalled = false;
    c.setVisible(true);
    harness.check(c.repaintCalled, false);
    // Change state and check if repaint is called.
    c.repaintCalled = false;
    c.setVisible(false);
    harness.check(c.repaintCalled, true);
    // Don't change state.
    c.repaintCalled = false;
    c.setVisible(false);
    harness.check(c.repaintCalled, false);
    // clean up the frame from desktop
    f.dispose();
  }
  
  /**
   * Tests if setVisible triggers a repaint when the component is not showing.
   *
   * @param harness the test harness to use
   */
  private void testRepaintNotShowing(TestHarness harness)
  {
    TestComponent c = new TestComponent();
    // Set to false, so that we know the state.
    c.setVisible(false);
    c.repaintCalled = false;
    // Change state and check if repaint is called.
    c.setVisible(true);
    harness.check(c.repaintCalled, false);
    // Don't change state.
    c.repaintCalled = false;
    c.setVisible(true);
    harness.check(c.repaintCalled, false);
    // Change state and check if repaint is called.
    c.repaintCalled = false;
    c.setVisible(false);
    harness.check(c.repaintCalled, false);
    // Don't change state.
    c.repaintCalled = false;
    c.setVisible(false);
    harness.check(c.repaintCalled, false);
  }

  /**
   * Tests if setVisible triggers a revalidate.
   *
   * @param harness the test harness to use
   */
  private void testRevalidate(TestHarness harness)
  {
    TestComponent c = new TestComponent();
    c.setBounds(10, 20, 30, 40);
    // Set to false, so that we know the state.
    c.setVisible(false);
    c.revalidateCalled = false;
    // Change state and check if repaint is called.
    c.setVisible(true);
    harness.check(c.revalidateCalled, true);
    // Don't change state.
    c.revalidateCalled = false;
    c.setVisible(true);
    harness.check(c.revalidateCalled, false);
    // Change state and check if repaint is called.
    c.revalidateCalled = false;
    c.setVisible(false);
    harness.check(c.revalidateCalled, true);
    // Don't change state.
    c.revalidateCalled = false;
    c.setVisible(false);
    harness.check(c.revalidateCalled, false);
  }
}
