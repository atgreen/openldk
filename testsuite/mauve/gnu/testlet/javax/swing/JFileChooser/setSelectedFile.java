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
import java.util.List;

import javax.swing.JFileChooser;

/**
 * Some checks for the setSelectedFile() method of the 
 * {@link JFileChooser} class.
 */
public class setSelectedFile implements Testlet, PropertyChangeListener {

  List events = new java.util.ArrayList();
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    events.add(e);
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)       
  {
    testGeneral(harness);
    testNull(harness);
  }
  
  /**
   * A general test.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void testGeneral(TestHarness harness)       
  {
    harness.checkPoint("testGeneral()");
    JFileChooser fc = new JFileChooser();
    events.clear();
    fc.addPropertyChangeListener(this);
    File file = fc.getSelectedFile();
    harness.check(file, null);
    
    File f1 = new File("X");
    fc.setSelectedFile(f1);
    harness.check(fc.getSelectedFile(), f1);
    File[] files = fc.getSelectedFiles();
    harness.check(files.length, 0);

    harness.check(events.size(), 1);
    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(0);
    harness.check(e1.getPropertyName(), 
            JFileChooser.SELECTED_FILE_CHANGED_PROPERTY);
    harness.check(e1.getOldValue(), null);
    harness.check(e1.getNewValue(), f1);
    events.clear();   
    
    // repeat the same file to see if an event is generated...
    fc.setSelectedFile(f1);
    harness.check(fc.getSelectedFile(), f1);
    files = fc.getSelectedFiles();
    harness.check(files.length, 0);

    harness.check(events.size(), 0);  
  }
  
  /**
   * Test setting the selected files to null.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void testNull(TestHarness harness)       
  {
    harness.checkPoint("testNull()");
    JFileChooser fc = new JFileChooser();
    events.clear();
    fc.addPropertyChangeListener(this);
    File file = fc.getSelectedFile();
    harness.check(file, null);
    fc.setSelectedFile(null);
    harness.check(fc.getSelectedFile(), null);

    harness.check(events.size(), 1);
    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(0);
    harness.check(e1.getPropertyName(), 
            JFileChooser.SELECTED_FILE_CHANGED_PROPERTY);
    harness.check(e1.getOldValue(), null);
    harness.check(e1.getNewValue(), null);
    events.clear();    
  }
}
