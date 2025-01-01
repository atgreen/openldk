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
 * Tests the removePropertyChangeListener() methods in the 
 * {@link SwingPropertyChangeSupport} class.
 */
public class removePropertyChangeListener 
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
    
    // remove it again
    s.removePropertyChangeListener(this);
    listeners = s.getPropertyChangeListeners();
    harness.check(listeners.length, 0);
    
    // remove a listener that doesn't exist
    s.removePropertyChangeListener(this);
    
    // try a null argument
    s.removePropertyChangeListener(null);
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
    
    // remove it again
    s.removePropertyChangeListener("X", this);
    listeners = s.getPropertyChangeListeners("X");
    harness.check(listeners.length, 0);
    
    // remove a listener that doesn't exist
    s.removePropertyChangeListener("X", this);
    
    // according to the 1.5.0 spec, a null property name causes no action
    // or exception
    boolean pass = false;
    try
    {
      s.removePropertyChangeListener(null, this);
      pass = true;
    }
    catch (Exception e)
    {
      pass = false;
    }
    harness.check(pass);
    
    // try a null argument 2
    s.removePropertyChangeListener("X", null);
  }

  public void propertyChange(PropertyChangeEvent e)
  {
    // do nothing 
  }
}
