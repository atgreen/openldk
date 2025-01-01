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
import java.beans.PropertyChangeListenerProxy;

import javax.swing.event.SwingPropertyChangeSupport;

/**
 * Tests the getPropertyChangeListeners() method in the 
 * {@link SwingPropertyChangeSupport} class.
 */
public class getPropertyChangeListeners 
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
    harness.checkPoint("()");
    SwingPropertyChangeSupport s = new SwingPropertyChangeSupport(this);
    PropertyChangeListener[] listeners = s.getPropertyChangeListeners();
    harness.check(listeners.length, 0);
    
    // now add a listener
    s.addPropertyChangeListener(this);
    listeners = s.getPropertyChangeListeners();;
    harness.check(listeners.length, 1);
    harness.check(listeners[0], this);

    // listeners for specific properties should show up in the list wrapped
    // in PropertyChangeListenerProxy instances
    s.addPropertyChangeListener("X", this);
    listeners = s.getPropertyChangeListeners();
    harness.check(listeners.length, 2);
    PropertyChangeListenerProxy proxy 
        = (PropertyChangeListenerProxy) listeners[1];
    harness.check(proxy.getPropertyName(), "X");
    harness.check(proxy.getListener(), this);
  }

  private void test2(TestHarness harness) 
  {
    harness.checkPoint("(String)");
    SwingPropertyChangeSupport s = new SwingPropertyChangeSupport(this);
    PropertyChangeListener[] listeners = s.getPropertyChangeListeners("X");
    harness.check(listeners.length, 0);
    
    // now add a listener
    s.addPropertyChangeListener("X", this);
    listeners = s.getPropertyChangeListeners("X");
    harness.check(listeners.length, 1);
    harness.check(listeners[0], this);
  }

  public void propertyChange(PropertyChangeEvent e)
  {
    // do nothing 
  }
}
