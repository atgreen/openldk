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
import java.util.List;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

/**
 * Some checks for the setAcceptAllFileFilterUsed() method of the 
 * {@link JFileChooser} class.
 */
public class setAcceptAllFileFilterUsed implements Testlet, PropertyChangeListener {

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
    JFileChooser fc = new JFileChooser();
    FileFilter acceptAllFilter = fc.getAcceptAllFileFilter();
    fc.addPropertyChangeListener(this);
    harness.check(fc.isAcceptAllFileFilterUsed(), true);
    MyFileFilter f1 = new MyFileFilter(true);
    fc.addChoosableFileFilter(f1);
    events.clear();
    
    fc.setAcceptAllFileFilterUsed(false);
    harness.check(fc.isAcceptAllFileFilterUsed(), false);
    harness.check(events.size(), 2);
    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(0);
    harness.check(e1.getPropertyName(), 
            JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY);
    FileFilter[] oldFilters = (FileFilter[]) e1.getOldValue();
    harness.check(oldFilters.length, 2);
    harness.check(oldFilters[0], acceptAllFilter);
    harness.check(oldFilters[1], f1);
    FileFilter[] newFilters = (FileFilter[]) e1.getNewValue();
    harness.check(newFilters.length, 1);
    harness.check(newFilters[0], f1);

    PropertyChangeEvent e2 = (PropertyChangeEvent) events.get(1);
    harness.check(e2.getPropertyName(), 
            JFileChooser.ACCEPT_ALL_FILE_FILTER_USED_CHANGED_PROPERTY);

    // also check if the 'accept all' filter is removed from the choosable
    // filters list
    FileFilter[] filters = fc.getChoosableFileFilters();
    harness.check(filters.length, 1);
    harness.check(filters[0], f1);
    events.clear();
    
    FileFilter ff = new MyFileFilter(false);
    fc.addChoosableFileFilter(ff);
    events.clear();
    harness.check(fc.getFileFilter(), ff);
    fc.setAcceptAllFileFilterUsed(true);
    harness.check(fc.isAcceptAllFileFilterUsed(), true);
    harness.check(fc.getFileFilter(), fc.getAcceptAllFileFilter());
    
    // expect 3 events
    harness.check(events.size(), 3);
    e1 = (PropertyChangeEvent) events.get(0);
    harness.check(e1.getPropertyName(),
            JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY);
    
    e2 = (PropertyChangeEvent) events.get(1);
    harness.check(e2.getPropertyName(), 
            JFileChooser.FILE_FILTER_CHANGED_PROPERTY);
//    harness.check(e1.getOldValue(), Boolean.FALSE);
//    harness.check(e1.getNewValue(), Boolean.TRUE);
    PropertyChangeEvent e3 = (PropertyChangeEvent) events.get(2);
    harness.check(e3.getPropertyName(), 
            JFileChooser.ACCEPT_ALL_FILE_FILTER_USED_CHANGED_PROPERTY);

    filters = fc.getChoosableFileFilters();
    harness.check(filters.length, 3);
  }

}
