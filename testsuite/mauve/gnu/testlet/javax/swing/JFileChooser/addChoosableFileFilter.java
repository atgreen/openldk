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
 * Some checks for the addChoosableFileFilter() method of the 
 * {@link JFileChooser} class.
 */
public class addChoosableFileFilter implements Testlet, PropertyChangeListener {

  List events = new java.util.ArrayList();;
  
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
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void testGeneral(TestHarness harness)       
  {
    harness.checkPoint("testGeneral()");
    JFileChooser fc = new JFileChooser();
    fc.addPropertyChangeListener(this);
    FileFilter f1 = new MyFileFilter(true);
    fc.addChoosableFileFilter(f1);
    FileFilter[] filters = fc.getChoosableFileFilters();
    harness.check(filters.length, 2);
    harness.check(filters[0], fc.getAcceptAllFileFilter());
    harness.check(filters[1], f1);
    harness.check(events.size(), 2);
    
    // adding the new filter should have generated two events, one for
    // the addition of a choosable file filter, and one for making it the
    // selected filter...
    PropertyChangeEvent event1 = (PropertyChangeEvent) events.get(0);
    harness.check(event1.getPropertyName(), 
            JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY);
    FileFilter[] oldFilters = (FileFilter[]) event1.getOldValue();
    harness.check(oldFilters.length, 1);
    harness.check(oldFilters[0], fc.getAcceptAllFileFilter());
    FileFilter[] newFilters = (FileFilter[]) event1.getNewValue();
    harness.check(newFilters.length, 2);
    harness.check(newFilters[0], fc.getAcceptAllFileFilter());
    harness.check(newFilters[1], f1);
    PropertyChangeEvent event2 = (PropertyChangeEvent) events.get(1);
    harness.check(event2.getPropertyName(), 
            JFileChooser.FILE_FILTER_CHANGED_PROPERTY);
    harness.check(event2.getOldValue(), fc.getAcceptAllFileFilter());
    harness.check(event2.getNewValue(), f1);
    events.clear();
  }
  
  /**
   * In this test, a null filter is added.
   * 
   * @param harness  the test harness.
   */
  public void testNull(TestHarness harness)
  {
    harness.checkPoint("testNull()");
    JFileChooser fc = new JFileChooser();
    events.clear();
    fc.addPropertyChangeListener(this);
    fc.addChoosableFileFilter(null);
    FileFilter[] filters = fc.getChoosableFileFilters();
    harness.check(filters.length, 1);
    harness.check(filters[0], fc.getAcceptAllFileFilter());
    harness.check(events.size(), 1);
    
    // adding the new filter should have generated two events, one for
    // the addition of a choosable file filter, and one for making it the
    // selected filter...
    PropertyChangeEvent event1 = (PropertyChangeEvent) events.get(0);
    harness.check(event1.getPropertyName(), 
            JFileChooser.FILE_FILTER_CHANGED_PROPERTY);
    harness.check(event1.getOldValue(), fc.getAcceptAllFileFilter());
    harness.check(event1.getNewValue(), null);
    events.clear();
  }

}
