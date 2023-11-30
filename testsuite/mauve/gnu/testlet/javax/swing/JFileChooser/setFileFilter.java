// Tags: JDK1.2
// Uses: MyFileFilter

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
import java.util.List;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

/**
 * Some checks for the setFileFilter() method of the 
 * {@link JFileChooser} class.
 */
public class setFileFilter implements Testlet, PropertyChangeListener {

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
    test1(harness);
    test2(harness);
  }
  
  public void test1(TestHarness harness)
  {
    harness.checkPoint("test1");
    JFileChooser fc = new JFileChooser();
    FileFilter acceptAllFilter = fc.getAcceptAllFileFilter();
    fc.addPropertyChangeListener(this);
    harness.check(fc.getFileFilter(), acceptAllFilter);
    
    FileFilter ff1 = new MyFileFilter(true);
    fc.setFileFilter(ff1);
    harness.check(fc.getFileFilter(), ff1);
    harness.check(events.size(), 2);
    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(0);
    harness.check(e1.getPropertyName(), 
            JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY);
    FileFilter[] filters1 = (FileFilter[]) e1.getOldValue();
    harness.check(filters1.length, 1);
    FileFilter[] filters2 = (FileFilter[]) e1.getNewValue();
    harness.check(filters2.length, 2);
    PropertyChangeEvent e2 = (PropertyChangeEvent) events.get(1);
    harness.check(e2.getPropertyName(), 
            JFileChooser.FILE_FILTER_CHANGED_PROPERTY);
    harness.check(e2.getOldValue(), acceptAllFilter);
    harness.check(e2.getNewValue(), ff1);
    events.clear();
  }

  public void test2(TestHarness harness)
  {
    harness.checkPoint("test2");
    JFileChooser fc = new JFileChooser();
    FileFilter acceptAllFilter = fc.getAcceptAllFileFilter();
    fc.addPropertyChangeListener(this);
    harness.check(fc.getFileFilter(), acceptAllFilter);
    
    fc.setFileFilter(null);
    harness.check(fc.getFileFilter(), null);
    harness.check(events.size(), 1);
    PropertyChangeEvent event = (PropertyChangeEvent) events.get(0);
    harness.check(event.getPropertyName(), 
            JFileChooser.FILE_FILTER_CHANGED_PROPERTY);
    harness.check(event.getOldValue(), acceptAllFilter);
    harness.check(event.getNewValue(), null);
  }
}
