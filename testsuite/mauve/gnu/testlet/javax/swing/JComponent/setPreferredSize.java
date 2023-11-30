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

import java.awt.Dimension;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JComponent;
import javax.swing.JPanel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests if setPreferredSize works correctly.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class setPreferredSize implements Testlet, PropertyChangeListener
{
    
  PropertyChangeEvent event;
  
  public void propertyChange(PropertyChangeEvent event) 
  {
    this.event = event;
  }
  
  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testGeneral(harness);
    testPropertyChangeEvent(harness);
    testRepaint(harness);
    testRevalidate(harness);
  }

  /**
   * Some general checks.
   * 
   * @param harness  the test harness.
   */
  private void testGeneral(TestHarness harness) 
  {
    JComponent c = new JPanel();
    harness.check(c.getPreferredSize(), new Dimension(10, 10));
    Dimension d = new Dimension(123, 456);
    c.setPreferredSize(d);
    harness.check(c.getPreferredSize(), d);
    harness.check(c.getPreferredSize() != d);
    c.setPreferredSize(null);  // restores the default
    harness.check(c.getPreferredSize(), new Dimension(10, 10));
  }
  
  private void testPropertyChangeEvent(TestHarness harness) 
  {
    JComponent c = new JPanel();
    c.addPropertyChangeListener(this);
    c.setPreferredSize(new Dimension(1, 2));
    harness.check(this.event.getPropertyName(), "preferredSize");
    harness.check(this.event.getOldValue(), null);
    harness.check(this.event.getNewValue(), new Dimension(1, 2));
    this.event = null;
    c.setPreferredSize(null);
    harness.check(this.event.getOldValue(), new Dimension(1, 2));
    harness.check(this.event.getNewValue(), null);
    this.event = null;
    c.setPreferredSize(null);
    harness.check(this.event.getOldValue(), null);
    harness.check(this.event.getNewValue(), null);
    c.setPreferredSize(new Dimension(12, 34));
    this.event = null;
    c.setPreferredSize(new Dimension(12, 34));
    harness.check(this.event, null);
  }
  /**
   * Tests if setPreferredSize triggers a repaint.
   *
   * @param harness the test harness to use
   */
  private void testRepaint(TestHarness harness)
  {
    Dimension s1 = new Dimension(100, 100);
    Dimension s2 = new Dimension(200, 200);
    TestComponent c = new TestComponent();
    // Set to s1, so that we know the state.
    c.setPreferredSize(s1);
    c.repaintCalled = false;
    // Change state and check if repaint is called.
    c.setPreferredSize(s2);
    harness.check(c.repaintCalled, false);
    // Don't change state.
    c.repaintCalled = false;
    c.setPreferredSize(s2);
    harness.check(c.repaintCalled, false);
    // Change state and check if repaint is called.
    c.repaintCalled = false;
    c.setPreferredSize(s1);
    harness.check(c.repaintCalled, false);
    // Don't change state.
    c.repaintCalled = false;
    c.setPreferredSize(s1);
    harness.check(c.repaintCalled, false);
  }

  /**
   * Tests if setPreferredSize triggers a revalidate.
   *
   * @param harness the test harness to use
   */
  private void testRevalidate(TestHarness harness)
  {
    Dimension s1 = new Dimension(100, 100);
    Dimension s2 = new Dimension(200, 200);
    TestComponent c = new TestComponent();
    // Set to s1, so that we know the state.
    c.setPreferredSize(s1);
    c.revalidateCalled = false;
    // Change state and check if repaint is called.
    c.setPreferredSize(s2);
    harness.check(c.revalidateCalled, false);
    // Don't change state.
    c.revalidateCalled = false;
    c.setPreferredSize(s2);
    harness.check(c.revalidateCalled, false);
    // Change state and check if repaint is called.
    c.revalidateCalled = false;
    c.setPreferredSize(s1);
    harness.check(c.revalidateCalled, false);
    // Don't change state.
    c.revalidateCalled = false;
    c.setPreferredSize(s1);
    harness.check(c.revalidateCalled, false);
  }
}
