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

package gnu.testlet.javax.swing.JComboBox;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JComboBox;

/**
 * Some checks for the setPrototypeDisplayValue() method in the 
 * {@link JComboBox} class.
 */
public class setPrototypeDisplayValue 
  implements Testlet, PropertyChangeListener 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {   
    JComboBox c = new JComboBox();
    c.addPropertyChangeListener(this);
    
    // set a new prototype
    c.setPrototypeDisplayValue("Test1");
    harness.check(c.getPrototypeDisplayValue(), "Test1");
    harness.check(name, "prototypeDisplayValue");
    harness.check(oldValue, null);
    harness.check(newValue, "Test1");
    
    // overwrite the existing prototype
    c.setPrototypeDisplayValue("Test2");
    harness.check(c.getPrototypeDisplayValue(), "Test2");
    harness.check(name, "prototypeDisplayValue");
    harness.check(oldValue, "Test1");
    harness.check(newValue, "Test2");
    
    // clear the prototype
    c.setPrototypeDisplayValue(null);
    harness.check(c.getPrototypeDisplayValue(), null);
    harness.check(name, "prototypeDisplayValue");
    harness.check(oldValue, "Test2");
    harness.check(newValue, null);
  }
  
  private String name = null;
  
  private Object oldValue = null;
  
  private Object newValue = null;
  
  public void propertyChange(PropertyChangeEvent e)
  {
    name = e.getPropertyName();
    oldValue = e.getOldValue();
    newValue = e.getNewValue();
  }
  

}

