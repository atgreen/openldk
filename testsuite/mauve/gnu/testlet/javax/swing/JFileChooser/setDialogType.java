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

/**
 * Some checks for the setDialogType() method of the 
 * {@link JFileChooser} class.
 */
public class setDialogType implements Testlet, PropertyChangeListener {

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
    harness.check(jfc.getDialogType(), JFileChooser.OPEN_DIALOG);
    
    jfc.setDialogType(JFileChooser.SAVE_DIALOG);
    harness.check(jfc.getDialogType(), JFileChooser.SAVE_DIALOG);
    harness.check(event.getPropertyName(), 
            JFileChooser.DIALOG_TYPE_CHANGED_PROPERTY);
    harness.check(event.getOldValue(), new Integer(JFileChooser.OPEN_DIALOG));
    harness.check(event.getNewValue(), new Integer(JFileChooser.SAVE_DIALOG));
    
    jfc.setDialogType(JFileChooser.CUSTOM_DIALOG);
    harness.check(jfc.getDialogType(), JFileChooser.CUSTOM_DIALOG);
    harness.check(event.getPropertyName(), 
            JFileChooser.DIALOG_TYPE_CHANGED_PROPERTY);
    harness.check(event.getOldValue(), new Integer(JFileChooser.SAVE_DIALOG));
    harness.check(event.getNewValue(), new Integer(JFileChooser.CUSTOM_DIALOG));
  }

}
