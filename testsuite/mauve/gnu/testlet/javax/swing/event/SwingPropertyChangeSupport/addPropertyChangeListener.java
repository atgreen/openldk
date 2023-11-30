// Tags: JDK1.4

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.javax.swing.event.SwingPropertyChangeSupport;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.event.SwingPropertyChangeSupport;

/**
 * Tests the addPropertyChangeListener() method in the 
 * {@link SwingPropertyChangeSupport} class.
 */
public class addPropertyChangeListener 
    implements Testlet, PropertyChangeListener {
  
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    test1(harness);
    test2(harness);
  }
  
  private void test1(TestHarness harness)
  {
    harness.checkPoint("(PropertyChangeListener)");
    SwingPropertyChangeSupport s = new SwingPropertyChangeSupport(this);
    
    // now add a listener
    s.addPropertyChangeListener(this);
    PropertyChangeListener[] listeners = s.getPropertyChangeListeners();
    harness.check(listeners.length, 1);
    harness.check(listeners[0], this);
    
    // try adding a null listener - it gets silently ignored
    s.addPropertyChangeListener(null);
    listeners = s.getPropertyChangeListeners();
    harness.check(listeners.length, 1);
  }

  private void test2(TestHarness harness)
  {
    harness.checkPoint("(String, PropertyChangeListener)");
    SwingPropertyChangeSupport s = new SwingPropertyChangeSupport(this);
    
    // now add a listener
    s.addPropertyChangeListener("X", this);
    PropertyChangeListener[] listeners = s.getPropertyChangeListeners("X");
    harness.check(listeners.length, 1);
    harness.check(listeners[0], this);
    
    // try adding a null listener - it gets silently ignored
    s.addPropertyChangeListener("X", null);
    listeners = s.getPropertyChangeListeners("X");
    harness.check(listeners.length, 1);
  }

  public void propertyChange(PropertyChangeEvent e)
  {
    // do nothing 
  }
}
