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
 * Tests the constructor in the {@link SwingPropertyChangeSupport} 
 * class.
 */
public class constructor implements Testlet, PropertyChangeListener {
  
  private PropertyChangeEvent event = null;
  
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    SwingPropertyChangeSupport s = new SwingPropertyChangeSupport(this);
    s.addPropertyChangeListener(this);
    s.firePropertyChange("X", false, true);
    harness.check(this.event.getSource(), this);
    
    // check null argument
    boolean pass = false;
    try
    {
      s = new SwingPropertyChangeSupport(null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  public void propertyChange(PropertyChangeEvent e)
  {
    this.event = e;
  }
}
