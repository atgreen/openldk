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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.JFileChooser;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JFileChooser;
import javax.swing.JPanel;

/**
 * Some checks for the setAccessory() method of the 
 * {@link JFileChooser} class.
 */
public class setAccessory implements Testlet, PropertyChangeListener {

  PropertyChangeEvent event;
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    event = e;
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)       
  {
    JFileChooser jfc = new JFileChooser();
    jfc.addPropertyChangeListener(this);
    harness.check(jfc.getAccessory(), null);
    
    JPanel acc1 = new JPanel();
    jfc.setAccessory(acc1);
    harness.check(jfc.getAccessory(), acc1);
    harness.check(event.getPropertyName(), 
            JFileChooser.ACCESSORY_CHANGED_PROPERTY);
    harness.check(event.getOldValue(), null);
    harness.check(event.getNewValue(), acc1);
    
    jfc.setAccessory(null);
    harness.check(jfc.getAccessory(), null);
    harness.check(event.getPropertyName(), 
            JFileChooser.ACCESSORY_CHANGED_PROPERTY);
    harness.check(event.getOldValue(), acc1);
    harness.check(event.getNewValue(), null);
  }

}
