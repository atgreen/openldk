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
 * Tests the firePropertyChange() methods in the 
 * {@link SwingPropertyChangeSupport} class.
 */
public class firePropertyChange 
    implements Testlet, PropertyChangeListener {
  
  private PropertyChangeEvent lastEvent = null;
  
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    test1(harness);
    test2(harness);
    test3(harness);
    test4(harness);
  }
  
  private void test1(TestHarness harness) 
  {
    harness.checkPoint("(PropertyChangeEvent)");
    SwingPropertyChangeSupport s = new SwingPropertyChangeSupport(this);
    s.addPropertyChangeListener(this);
    PropertyChangeEvent e = new PropertyChangeEvent("SOURCE", "X", "Y", "Z");
    s.firePropertyChange(e);
    harness.check(this.lastEvent.getSource(), "SOURCE");
    harness.check(this.lastEvent.getPropertyName(), "X");
    harness.check(this.lastEvent.getOldValue(), "Y");
    harness.check(this.lastEvent.getNewValue(), "Z");
    
    // if the old and new values are the same (and non-null), then no listeners
    // are notified
    this.lastEvent = null;
    e = new PropertyChangeEvent("SOURCE", "X", "YY", "YY");
    s.firePropertyChange(e);
    harness.check(lastEvent, null);
    
    // but if the old and new values are both null, listeners ARE notified
    this.lastEvent = null;
    e = new PropertyChangeEvent("SOURCE", "X", null, null);
    s.firePropertyChange(e);
    harness.check(lastEvent, e);    
    
    // check that a null argument throws a null pointer exception
    this.lastEvent = null;
    boolean pass = false;
    try
    {
      s.firePropertyChange(null);
    }
    catch (NullPointerException npe)
    {
      pass = true;
    }
    harness.check(pass);
  }

  private void test2(TestHarness harness) 
  {
    harness.checkPoint("(String, Object, Object)");
    SwingPropertyChangeSupport s = new SwingPropertyChangeSupport("SOURCE");
    s.addPropertyChangeListener(this);
    s.firePropertyChange("X", "Y", "Z");
    harness.check(this.lastEvent.getSource(), "SOURCE");
    harness.check(this.lastEvent.getPropertyName(), "X");
    harness.check(this.lastEvent.getOldValue(), "Y");
    harness.check(this.lastEvent.getNewValue(), "Z");
    this.lastEvent = null;
    
    // if both Objects are equal and non-null, no event is generated
    this.lastEvent = null;
    s.firePropertyChange("X", "Z", "Z");
    harness.check(this.lastEvent, null);

    // the following should not throw any exceptions
//    s.firePropertyChange(null, "Y", "Z");
//    s.firePropertyChange("X", null, "Z");
//    s.firePropertyChange("X", "Y", null);
  }

  private void test3(TestHarness harness) 
  {
    harness.checkPoint("(String, boolean, boolean)");
    SwingPropertyChangeSupport s = new SwingPropertyChangeSupport("SOURCE");
    s.addPropertyChangeListener(this);
    s.firePropertyChange("X", false, true);
    harness.check(this.lastEvent.getSource(), "SOURCE");
    harness.check(this.lastEvent.getPropertyName(), "X");
    harness.check(this.lastEvent.getOldValue(), Boolean.FALSE);
    harness.check(this.lastEvent.getNewValue(), Boolean.TRUE);
    
    // if both booleans are equal, no event is generated
    this.lastEvent = null;
    s.firePropertyChange("X", true, true);
    harness.check(this.lastEvent, null);
  }

  private void test4(TestHarness harness) 
  {
    harness.checkPoint("(String, int, int)");
    SwingPropertyChangeSupport s = new SwingPropertyChangeSupport("SOURCE");
    s.addPropertyChangeListener(this);
    s.firePropertyChange("X", 12, 34);
    harness.check(this.lastEvent.getSource(), "SOURCE");
    harness.check(this.lastEvent.getPropertyName(), "X");
    harness.check(this.lastEvent.getOldValue(), new Integer(12));
    harness.check(this.lastEvent.getNewValue(), new Integer(34));
    
    // if both ints are equal, no event is generated
    this.lastEvent = null;
    s.firePropertyChange("X", 99, 99);
    harness.check(this.lastEvent, null);
  }

  public void propertyChange(PropertyChangeEvent e)
  {
    this.lastEvent = e;
  }
}
