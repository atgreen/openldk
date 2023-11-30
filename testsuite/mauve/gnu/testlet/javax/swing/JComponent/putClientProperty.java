// Tags: JDK1.2

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

package gnu.testlet.javax.swing.JComponent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JComponent;
import javax.swing.JLabel;

/**
 * Some checks for the putClientProperty() method in the {@link JComponent} 
 * class.
 */
public class putClientProperty implements Testlet, PropertyChangeListener 
{

  public String name = null;
  
  public Object oldValue = null;
  
  public Object newValue = null;
  
  public void propertyChange(PropertyChangeEvent e)
  {
    name = e.getPropertyName();
    oldValue = e.getOldValue();
    newValue = e.getNewValue();
  }
  
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {   
    JComponent label = new JLabel("Test");
    label.addPropertyChangeListener(this);

    // add a new property
    label.putClientProperty("Property1", Boolean.TRUE);
    harness.check(label.getClientProperty("Property1"), Boolean.TRUE);
    harness.check(name, "Property1");
    harness.check(oldValue, null);
    harness.check(newValue, Boolean.TRUE);

    // Set testnull to null. No event is fired.
    label.putClientProperty("testnull", null);
    name = null;
    oldValue = null;
    newValue = null;
    label.putClientProperty("testnull", null);
    harness.check(name, null);
    harness.check(oldValue, null);
    harness.check(newValue, null);

    // overwrite an existing property
    label.putClientProperty("Property1", Boolean.FALSE);
    harness.check(label.getClientProperty("Property1"), Boolean.FALSE);
    harness.check(name, "Property1");
    harness.check(oldValue, Boolean.TRUE);
    harness.check(newValue, Boolean.FALSE);
    
    // clear the property
    label.putClientProperty("Property1", null);
    harness.check(label.getClientProperty("Property1"), null);
    harness.check(name, "Property1");
    harness.check(oldValue, Boolean.FALSE);
    harness.check(newValue, null);
    
    // try a null key
    boolean pass = false;
    try
    {
      label.putClientProperty(null, "XYZ");
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}

