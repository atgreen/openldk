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
import java.io.File;

import javax.swing.JFileChooser;

/**
 * Some checks for the setCurrentDirectory() method of the 
 * {@link JFileChooser} class.
 */
public class setCurrentDirectory implements Testlet, PropertyChangeListener {

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
    JFileChooser fc = new JFileChooser();
    fc.addPropertyChangeListener(this);
    harness.check(fc.getCurrentDirectory(), new File(System.getProperty("user.home")));
    
    File d = new File(harness.getTempDirectory());
    fc.setCurrentDirectory(d);
    harness.check(fc.getCurrentDirectory(), d);
    harness.check(event.getPropertyName(), JFileChooser.DIRECTORY_CHANGED_PROPERTY);
    harness.check(event.getOldValue(), new File(System.getProperty("user.home")));
    harness.check(event.getNewValue(), d);
    
    fc.setCurrentDirectory(null);
    harness.check(fc.getCurrentDirectory(), new File(System.getProperty("user.home")));
    harness.check(event.getPropertyName(), JFileChooser.DIRECTORY_CHANGED_PROPERTY);
    harness.check(event.getOldValue(), d);
    harness.check(event.getNewValue(), new File(System.getProperty("user.home")));
  }

}
