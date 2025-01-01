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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.JComboBox;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JComboBox;

/**
 * Some checks for the setEditable() method in the 
 * {@link JComboBox} class.
 */
public class setEditable 
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
    c.setEditable(false);
    harness.check(c.isEditable(), false);
    
    // now check property change events
    c.addPropertyChangeListener(this);
    
    c.setEditable(true);
    harness.check(c.isEditable(), true);
    harness.check(event.getPropertyName(), "editable");
    harness.check(event.getOldValue(), Boolean.FALSE);
    harness.check(event.getNewValue(), Boolean.TRUE);
    
    c.setEditable(false);
    harness.check(c.isEditable(), false);
    harness.check(event.getPropertyName(), "editable");
    harness.check(event.getOldValue(), Boolean.TRUE);
    harness.check(event.getNewValue(), Boolean.FALSE);
    
    // no change = no event
    event = null;
    
    c.setEditable(false);
    harness.check(event == null);
  }
  
  private PropertyChangeEvent event;
  
  public void propertyChange(PropertyChangeEvent e)
  {
    event = e;
  }
  

}

