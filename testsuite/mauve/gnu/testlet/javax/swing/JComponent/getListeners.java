// Tags: JDK1.3

// Copyright (C) 2005, 2006 David Gilbert  <david.gilbert@object-refinery.com>

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.JComponent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.event.FocusListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.util.EventListener;

import javax.swing.JComponent;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;

/**
 * Some tests for the <code>getListeners(Class)</code> method in the 
 * {@link JComponent} class.
 */
public class getListeners implements Testlet, AncestorListener {

  class TestComponent extends JComponent
  {
  }
  
  class MyPropertyChangeListener 
    implements PropertyChangeListener
  {
    public void propertyChange(PropertyChangeEvent e) 
    {
      // ignore
    }
  }
  
  class MyVetoableChangeListener implements VetoableChangeListener
  {
    public void vetoableChange(PropertyChangeEvent e) 
      throws PropertyVetoException 
    {
      // ignore
    }
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    TestComponent c = new TestComponent();
    c.addAncestorListener(this);
    EventListener[] listeners = c.getListeners(AncestorListener.class);
    harness.check(listeners.length, 1);
    harness.check(listeners[0], this);
    
    // try a listener type that isn't registered
    listeners = c.getListeners(FocusListener.class);
    harness.check(listeners.length, 0);
    c.removeAncestorListener(this);
    listeners = c.getListeners(AncestorListener.class);
    harness.check(listeners.length, 0);
    
    // try a PropertyChangeListener
    PropertyChangeListener pcl = new MyPropertyChangeListener();
    c.addPropertyChangeListener(pcl);
    listeners = c.getListeners(PropertyChangeListener.class);
    harness.check(listeners.length, 1);
    if (listeners.length > 0) 
      harness.check(listeners[0], pcl);
    else
      harness.check(false);
    
    // try a VetoableChangeListener
    VetoableChangeListener vcl = new MyVetoableChangeListener();
    c.addVetoableChangeListener(vcl);
    listeners = c.getListeners(VetoableChangeListener.class);
    harness.check(listeners.length, 1);
    if (listeners.length > 0) 
      harness.check(listeners[0], vcl);
    else
      harness.check(false);
    
    // try a null argument
    boolean pass = false;
    try
    {
      listeners = c.getListeners(null);
    }
    catch (NullPointerException e) 
    {
      pass = true;    
    }
    harness.check(pass);
    
  /* Doesn't compile with 1.5
    // try a class that isn't a listener
    pass = false;
    try
    {
      listeners = c.getListeners(Integer.class);
    }
    catch (ClassCastException e) 
    {
      pass = true;
    }
    harness.check(pass);
  */
  }
  
  public void ancestorMoved(AncestorEvent e)
  {
    // ignored
  }

  public void ancestorAdded(AncestorEvent e)
  {
    // ignored
  }
  
  public void ancestorRemoved(AncestorEvent e)
  {
    // ignored
  }
}
