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

import java.awt.Dimension;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JComponent;
import javax.swing.JPanel;

/**
 * Tests if setMaximumSize works correctly.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class setMaximumSize implements Testlet, PropertyChangeListener
{
    
  PropertyChangeEvent event;
  
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
    harness.check(c.getMaximumSize(), new Dimension(32767, 32767));
    Dimension d = new Dimension(123, 456);
    c.setMaximumSize(d);
    harness.check(c.getMaximumSize(), d);
    harness.check(c.getMaximumSize() != d);
    c.setMaximumSize(null);  // restores the default
    harness.check(c.getMaximumSize(), new Dimension(32767, 32767));
  }
  
  private void testPropertyChangeEvent(TestHarness harness) 
  {
    JComponent c = new JPanel();
    c.addPropertyChangeListener(this);
    c.setMaximumSize(new Dimension(1, 2));
    harness.check(this.event.getPropertyName(), "maximumSize");
    harness.check(this.event.getOldValue(), null);
    harness.check(this.event.getNewValue(), new Dimension(1, 2));
    this.event = null;
    c.setMaximumSize(null);
    harness.check(this.event.getOldValue(), new Dimension(1, 2));
    harness.check(this.event.getNewValue(), null);
  }
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    this.event = e;
  }
  
  /**
   * Tests if setMaximumSize triggers a repaint.
   *
   * @param harness the test harness to use
   */
  private void testRepaint(TestHarness harness)
  {
    Dimension s1 = new Dimension(100, 100);
    Dimension s2 = new Dimension(200, 200);
    TestComponent c = new TestComponent();
    // Set to s1, so that we know the state.
    c.setMaximumSize(s1);
    c.repaintCalled = false;
    // Change state and check if repaint is called.
    c.setMaximumSize(s2);
    harness.check(c.repaintCalled, false);
    // Don't change state.
    c.repaintCalled = false;
    c.setMaximumSize(s2);
    harness.check(c.repaintCalled, false);
    // Change state and check if repaint is called.
    c.repaintCalled = false;
    c.setMaximumSize(s1);
    harness.check(c.repaintCalled, false);
    // Don't change state.
    c.repaintCalled = false;
    c.setMaximumSize(s1);
    harness.check(c.repaintCalled, false);
  }

  /**
   * Tests if setMaximumSize triggers a revalidate.
   *
   * @param harness the test harness to use
   */
  private void testRevalidate(TestHarness harness)
  {
    Dimension s1 = new Dimension(100, 100);
    Dimension s2 = new Dimension(200, 200);
    TestComponent c = new TestComponent();
    // Set to false, so that we know the state.
    c.setMaximumSize(s1);
    c.revalidateCalled = false;
    // Change state and check if repaint is called.
    c.setMaximumSize(s2);
    harness.check(c.revalidateCalled, false);
    // Don't change state.
    c.revalidateCalled = false;
    c.setMaximumSize(s2);
    harness.check(c.revalidateCalled, false);
    // Change state and check if repaint is called.
    c.revalidateCalled = false;
    c.setMaximumSize(s1);
    harness.check(c.revalidateCalled, false);
    // Don't change state.
    c.revalidateCalled = false;
    c.setMaximumSize(s1);
    harness.check(c.revalidateCalled, false);
  }
}
