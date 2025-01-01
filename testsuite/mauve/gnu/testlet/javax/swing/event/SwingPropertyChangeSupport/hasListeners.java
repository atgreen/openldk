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
// along with Mauve; see the file COPYING.  If not, write to the
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301 USA.

package gnu.testlet.javax.swing.event.SwingPropertyChangeSupport;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.event.SwingPropertyChangeSupport;

/**
 * Tests the hasListeners() method in the {@link SwingPropertyChangeSupport} 
 * class.
 */
public class hasListeners implements Testlet, PropertyChangeListener {
  
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    SwingPropertyChangeSupport s = new SwingPropertyChangeSupport(this);
    harness.check(s.hasListeners("X"), false);
    
    // add a listener for all events
    s.addPropertyChangeListener(this);
    harness.check(s.hasListeners("X"));
    s.removePropertyChangeListener(this);
    harness.check(s.hasListeners("X"), false);
    
    // add a listener for a specific event
    s.addPropertyChangeListener("X", this);
    harness.check(s.hasListeners("X"), true);
    s.removePropertyChangeListener("X", this);
    harness.check(s.hasListeners("X"), false);
    
    // check null argument - in 1.5.0 the spec says that this checks for 
    // listeners registered against all properties
    harness.check(s.hasListeners(null), false);
    
    // add a listener for all events
    s.addPropertyChangeListener(this);
    harness.check(s.hasListeners(null));
    s.removePropertyChangeListener(this);
    harness.check(s.hasListeners(null), false); 
  }

  public void propertyChange(PropertyChangeEvent e)
  {
    // do nothing 
  }
}
