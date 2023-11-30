// Tags: JDK1.4

// Copyright (C) 2005, 2006 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.javax.swing.UIManager;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.UIManager;

/**
 * Tests the getPropertyChangeListeners() method in the {@link UIManager} class.
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
    // check that there are no listeners
    PropertyChangeListener[] listeners = UIManager.getPropertyChangeListeners();
    int count = listeners.length;
    
    // now add a listener
    UIManager.addPropertyChangeListener(this);
    listeners = UIManager.getPropertyChangeListeners();;
    harness.check(listeners.length, count + 1);
    harness.check(listeners[count], this);
    
  }

  public void propertyChange(PropertyChangeEvent e)
  {
    // do nothing - there are tests elsewhere that will verify that the event
    // is actually called
  }
}
