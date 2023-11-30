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
import javax.swing.UIManager;
import javax.swing.filechooser.FileFilter;

/**
 * Some checks for the removeChoosableFileFilter() method of the 
 * {@link JFileChooser} class.
 */
public class removeChoosableFileFilter implements Testlet, PropertyChangeListener {

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
    try
    {
      UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");      
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }
    test1(harness);
    test2(harness);
    test3(harness);
    test4(harness);
    test5(harness);
    test6(harness);
    test7(harness);
    test8(harness);
  }
  
  /**
   * In this test, 'accept all' is the only filter in the list, and we try
   * removing it.
   * 
   * @param harness
   */
  public void test1(TestHarness harness) 
  {
    harness.checkPoint("test1");
    JFileChooser fc = new JFileChooser();
    FileFilter acceptAllFilter = fc.getAcceptAllFileFilter();
    fc.addPropertyChangeListener(this);
    fc.removeChoosableFileFilter(acceptAllFilter);
    FileFilter[] filters = fc.getChoosableFileFilters();
    harness.check(filters.length, 0);
    harness.check(events.size(), 2);
    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(0);
    harness.check(e1.getPropertyName(), 
            JFileChooser.FILE_FILTER_CHANGED_PROPERTY);
    harness.check(e1.getOldValue(), acceptAllFilter);
    harness.check(e1.getNewValue(), null);
    PropertyChangeEvent e2 = (PropertyChangeEvent) events.get(1);
    harness.check(e2.getPropertyName(), 
            JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY);
    FileFilter[] ff1 = (FileFilter[]) e2.getOldValue();
    FileFilter[] ff2 = (FileFilter[]) e2.getNewValue();
    harness.check(ff1.length, 1);
    harness.check(ff2.length, 0);
    events.clear();
  }

  /**
   * In this test, 'accept all' is the selected filter, there is another
   * item in the list, and we remove 'accept all'.
   * @param harness
   */
  public void test2(TestHarness harness) 
  {
    harness.checkPoint("test2");   
    JFileChooser fc = new JFileChooser();
    FileFilter acceptAllFilter = fc.getAcceptAllFileFilter();
    FileFilter f1 = new MyFileFilter(true);
    fc.addChoosableFileFilter(f1);
    fc.setFileFilter(acceptAllFilter);
    fc.addPropertyChangeListener(this);
    boolean removed = fc.removeChoosableFileFilter(acceptAllFilter);
    harness.check(removed);
    FileFilter[] filters = fc.getChoosableFileFilters();
    harness.check(filters.length, 1);
    harness.check(events.size(), 2);
    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(0);
    harness.check(e1.getPropertyName(), 
            JFileChooser.FILE_FILTER_CHANGED_PROPERTY);
    harness.check(e1.getOldValue(), acceptAllFilter);
    harness.check(e1.getNewValue(), null);
    PropertyChangeEvent e2 = (PropertyChangeEvent) events.get(1);
    harness.check(e2.getPropertyName(), 
            JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY);
    FileFilter[] ff1 = (FileFilter[]) e2.getOldValue();
    FileFilter[] ff2 = (FileFilter[]) e2.getNewValue();
    harness.check(ff1.length, 2);
    harness.check(ff2.length, 1);
    harness.check(fc.getFileFilter(), null);
    events.clear();
  }
  
  /**
   * In this test, 'accept all' is the selected filter, there is another
   * item in the list, and we remove the other item.
   * 
   * @param harness
   */
  public void test3(TestHarness harness) 
  {
    harness.checkPoint("test3");   
    JFileChooser fc = new JFileChooser();
    FileFilter acceptAllFilter = fc.getAcceptAllFileFilter();
    FileFilter f1 = new MyFileFilter(true);
    fc.addChoosableFileFilter(f1);
    fc.setFileFilter(acceptAllFilter);
    fc.addPropertyChangeListener(this);
    boolean removed = fc.removeChoosableFileFilter(f1);
    harness.check(removed);
    FileFilter[] filters = fc.getChoosableFileFilters();
    harness.check(filters.length, 1);
    harness.check(events.size(), 1);
    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(0);
    harness.check(e1.getPropertyName(), 
            JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY);
    FileFilter[] ff1 = (FileFilter[]) e1.getOldValue();
    FileFilter[] ff2 = (FileFilter[]) e1.getNewValue();
    harness.check(ff1.length, 2);
    harness.check(ff2.length, 1);
    harness.check(fc.getFileFilter(), acceptAllFilter);
    events.clear();
  }
  
  /**
   * In this test, 'accept all' is in the list, there is another
   * item selected, and we remove 'accept all'.
   * @param harness
   */
  public void test4(TestHarness harness) 
  {
    harness.checkPoint("test4");   
    JFileChooser fc = new JFileChooser();
    FileFilter acceptAllFilter = fc.getAcceptAllFileFilter();
    FileFilter f1 = new MyFileFilter(true);
    fc.addChoosableFileFilter(f1);
    fc.addPropertyChangeListener(this);
    boolean removed = fc.removeChoosableFileFilter(acceptAllFilter);
    harness.check(removed);
    FileFilter[] filters = fc.getChoosableFileFilters();
    harness.check(filters.length, 1);
    harness.check(events.size(), 1);
    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(0);
    harness.check(e1.getPropertyName(), 
            JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY);
    FileFilter[] ff1 = (FileFilter[]) e1.getOldValue();
    FileFilter[] ff2 = (FileFilter[]) e1.getNewValue();
    harness.check(ff1.length, 2);
    harness.check(ff2.length, 1);
    harness.check(fc.getFileFilter(), f1);
    events.clear();
  }
  
  /**
   * In this test, 'accept all' is in the list, there is another
   * item selected, and we remove the selected item.
   * @param harness
   */
  public void test5(TestHarness harness) 
  {
    harness.checkPoint("test5");   
    JFileChooser fc = new JFileChooser();
    FileFilter f1 = new MyFileFilter(true);
    fc.addChoosableFileFilter(f1);
    fc.addPropertyChangeListener(this);
    boolean removed = fc.removeChoosableFileFilter(f1);
    harness.check(removed);
    FileFilter[] filters = fc.getChoosableFileFilters();
    harness.check(filters.length, 1);
    harness.check(events.size(), 2);
    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(0);
    harness.check(e1.getPropertyName(), 
            JFileChooser.FILE_FILTER_CHANGED_PROPERTY);
    harness.check(e1.getOldValue(), f1);
    harness.check(e1.getNewValue(), null);
    PropertyChangeEvent e2 = (PropertyChangeEvent) events.get(1);
    harness.check(e2.getPropertyName(), 
            JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY);
    FileFilter[] ff1 = (FileFilter[]) e2.getOldValue();
    FileFilter[] ff2 = (FileFilter[]) e2.getNewValue();
    harness.check(ff1.length, 2);
    harness.check(ff2.length, 1);
    harness.check(fc.getFileFilter(), null);
    events.clear();
  }
  
  /**
   * In this test, 'accept all' is not in use, there is only one item and we 
   * remove it.
   * 
   * @param harness
   */
  public void test6(TestHarness harness) 
  {
    harness.checkPoint("test6");   
    JFileChooser fc = new JFileChooser();
    FileFilter f1 = new MyFileFilter(true);
    fc.addChoosableFileFilter(f1);
    fc.setAcceptAllFileFilterUsed(false);
    fc.addPropertyChangeListener(this);
    boolean removed = fc.removeChoosableFileFilter(f1);
    harness.check(removed);
    FileFilter[] filters = fc.getChoosableFileFilters();
    harness.check(filters.length, 0);
    harness.check(events.size(), 2);
    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(0);
    harness.check(e1.getPropertyName(), 
            JFileChooser.FILE_FILTER_CHANGED_PROPERTY);
    harness.check(e1.getOldValue(), f1);
    harness.check(e1.getNewValue(), null);
    PropertyChangeEvent e2 = (PropertyChangeEvent) events.get(1);
    harness.check(e2.getPropertyName(), 
            JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY);
    FileFilter[] ff1 = (FileFilter[]) e2.getOldValue();
    FileFilter[] ff2 = (FileFilter[]) e2.getNewValue();
    harness.check(ff1.length, 1);
    harness.check(ff2.length, 0);
    harness.check(fc.getFileFilter(), null);
    events.clear();
  }
  
  /**
   * In this test, 'accept all' is not in use, there are two filters and we 
   * remove the selected filter.
   * 
   * @param harness
   */
  public void test7(TestHarness harness) 
  {
    harness.checkPoint("test7");   
    JFileChooser fc = new JFileChooser();
    FileFilter f1 = new MyFileFilter(true);
    FileFilter f2 = new MyFileFilter(false);
    fc.addChoosableFileFilter(f1);
    fc.addChoosableFileFilter(f2);
    fc.setAcceptAllFileFilterUsed(false);
    fc.addPropertyChangeListener(this);
    boolean removed = fc.removeChoosableFileFilter(f2);
    harness.check(removed);
    FileFilter[] filters = fc.getChoosableFileFilters();
    harness.check(filters.length, 1);
    harness.check(events.size(), 2);
    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(0);
    harness.check(e1.getPropertyName(), 
            JFileChooser.FILE_FILTER_CHANGED_PROPERTY);
    harness.check(e1.getOldValue(), f2);
    harness.check(e1.getNewValue(), null);
    PropertyChangeEvent e2 = (PropertyChangeEvent) events.get(1);
    harness.check(e2.getPropertyName(), 
            JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY);
    FileFilter[] ff1 = (FileFilter[]) e2.getOldValue();
    FileFilter[] ff2 = (FileFilter[]) e2.getNewValue();
    harness.check(ff1.length, 2);
    harness.check(ff2.length, 1);
    harness.check(fc.getFileFilter(), null);
    events.clear();
  }
  
  /**
   * In this test, 'accept all' is not in use, there are two filters and we 
   * remove the not-selected filter.
   * 
   * @param harness
   */
  public void test8(TestHarness harness) 
  {
    harness.checkPoint("test8");   
    JFileChooser fc = new JFileChooser();
    FileFilter f1 = new MyFileFilter(true);
    FileFilter f2 = new MyFileFilter(false);
    fc.addChoosableFileFilter(f1);
    fc.addChoosableFileFilter(f2);
    fc.setAcceptAllFileFilterUsed(false);
    fc.addPropertyChangeListener(this);
    boolean removed = fc.removeChoosableFileFilter(f1);
    harness.check(removed);
    FileFilter[] filters = fc.getChoosableFileFilters();
    harness.check(filters.length, 1);
    harness.check(events.size(), 1);
    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(0);
    harness.check(e1.getPropertyName(), 
            JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY);
    FileFilter[] ff1 = (FileFilter[]) e1.getOldValue();
    FileFilter[] ff2 = (FileFilter[]) e1.getNewValue();
    harness.check(ff1.length, 2);
    harness.check(ff2.length, 1);
    harness.check(fc.getFileFilter(), f2);
    events.clear();
  }
}
