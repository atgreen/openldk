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
import javax.swing.filechooser.FileView;

/**
 * Some checks for the setFileView() method of the 
 * {@link JFileChooser} class.
 */
public class setFileView implements Testlet, PropertyChangeListener {

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
    harness.check(jfc.getFileView(), null);
    
    FileView fv1 = new FileView() {};
    jfc.setFileView(fv1);
    harness.check(jfc.getFileView(), fv1);
    harness.check(event.getPropertyName(), 
            JFileChooser.FILE_VIEW_CHANGED_PROPERTY);
    harness.check(event.getOldValue(), null);
    harness.check(event.getNewValue(), fv1);
    
    jfc.setFileView(null);
    harness.check(jfc.getFileView(), null);
    harness.check(event.getPropertyName(), 
            JFileChooser.FILE_VIEW_CHANGED_PROPERTY);
    harness.check(event.getOldValue(), fv1);
    harness.check(event.getNewValue(), null);
  }

}
