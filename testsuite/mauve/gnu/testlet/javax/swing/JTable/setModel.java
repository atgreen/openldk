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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.JTable;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Arrays;

import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumnModel;

/**
 * Some checks for the setModel() method in the {@link JTable} class.
 */
public class setModel implements Testlet {

  class PropertyChangeHandler implements PropertyChangeListener
  {

    /**
     * Receives notification when a property changes.
     *
     * @param e the property change event
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      propertyChangeFired = true;
    }
    
  }

  boolean propertyChangeFired;

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    test1(harness);
    test2(harness);
    testLeadAnchorSelectionUpdate(harness);
    testSelectionModel(harness);
    testPropertyFired(harness);
  }

  public void test1(TestHarness harness)      
  {
    harness.checkPoint("setModel(TableModel) - test1");
    DefaultTableModel m1 = new DefaultTableModel();
    JTable t = new JTable(m1);
    DefaultTableModel m2 = new DefaultTableModel();
    t.setModel(m2);
    TableModelListener[] listeners1 = m1.getTableModelListeners();
    TableModelListener[] listeners2 = m2.getTableModelListeners();
    harness.check(!Arrays.asList(listeners1).contains(t));
    harness.check(Arrays.asList(listeners2).contains(t));
    
    // try a null argument
    boolean pass = false;
    try
    {
      t.setModel(null);   
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
  }

  /**
   * This test checks that the autoCreateColumnsFromModel flag is correctly
   * observed.
   * 
   * @param harness  the test harness.
   */
  public void test2(TestHarness harness)      
  {
    harness.checkPoint("setModel(TableModel) - test2");
    DefaultTableModel m1 = new DefaultTableModel(2, 3);
    JTable t = new JTable(m1);
    TableColumnModel tcm = t.getColumnModel();
    tcm.getColumn(1).setModelIndex(0);
    tcm.getColumn(0).setModelIndex(1);
    harness.check(t.getColumnCount(), 3);
    harness.check(t.getColumnName(0), "B");
    harness.check(t.getColumnName(1), "A");
    harness.check(t.getColumnName(2), "C");
    
    DefaultTableModel m2 = new DefaultTableModel(new String[] {"AA", "BB"}, 1);
    t.setModel(m2);
    harness.check(t.getColumnCount(), 2);
    harness.check(t.getColumnName(0), "AA");
    harness.check(t.getColumnName(1), "BB");
   
    tcm = t.getColumnModel();
    tcm.getColumn(1).setModelIndex(0);
    tcm.getColumn(0).setModelIndex(1);    
    t.setAutoCreateColumnsFromModel(false);
    DefaultTableModel m3 = new DefaultTableModel(
            new String[] {"CC", "DD", "EE"}, 1);
    t.setModel(m3);
    harness.check(t.getColumnCount(), 2);
    harness.check(t.getColumnName(0), "DD");
    harness.check(t.getColumnName(1), "CC");
  }

  /**
   * Tests if setModel updates the lead and anchor selection indices correctly.
   *
   * @param the test harness to use
   */
  private void testLeadAnchorSelectionUpdate(TestHarness harness)
  {
    harness.checkPoint("leadAnchorSelectionUpdate");

    JTable table = new JTable(0, 0);

    // Test a model with 0 rows and 0 columns.
    table.setModel(new DefaultTableModel(0, 0));
    try { Thread.sleep(500); } catch (InterruptedException ex) {}
    harness.check(table.getSelectionModel().getLeadSelectionIndex(), -1);
    harness.check(table.getSelectionModel().getAnchorSelectionIndex(), -1);
    harness.check(table.getColumnModel().getSelectionModel()
                  .getLeadSelectionIndex(), -1);
    harness.check(table.getColumnModel().getSelectionModel()
                  .getAnchorSelectionIndex(), -1);

    // Test a model with 1 row and 0 columns.
    table.setModel(new DefaultTableModel(1, 0));
    try { Thread.sleep(500); } catch (InterruptedException ex) {}
    harness.check(table.getSelectionModel().getLeadSelectionIndex(), 0);
    harness.check(table.getSelectionModel().getAnchorSelectionIndex(), 0);
    harness.check(table.getColumnModel().getSelectionModel()
                  .getLeadSelectionIndex(), -1);
    harness.check(table.getColumnModel().getSelectionModel()
                  .getAnchorSelectionIndex(), -1);

    // Test a model with 0 rows and 1 column.
    table.setModel(new DefaultTableModel(0, 1));
    try { Thread.sleep(500); } catch (InterruptedException ex) {}
    harness.check(table.getSelectionModel().getLeadSelectionIndex(), -1);
    harness.check(table.getSelectionModel().getAnchorSelectionIndex(), -1);
    harness.check(table.getColumnModel().getSelectionModel()
                  .getLeadSelectionIndex(), 0);
    harness.check(table.getColumnModel().getSelectionModel()
                  .getAnchorSelectionIndex(), 0);

    // Test a model with 1 row and 1 columns.
    table.setModel(new DefaultTableModel(1, 1));
    try { Thread.sleep(500); } catch (InterruptedException ex) {}
    harness.check(table.getSelectionModel().getLeadSelectionIndex(), 0);
    harness.check(table.getSelectionModel().getAnchorSelectionIndex(), 0);
    harness.check(table.getColumnModel().getSelectionModel()
                  .getLeadSelectionIndex(), 0);
    harness.check(table.getColumnModel().getSelectionModel()
                  .getAnchorSelectionIndex(), 0);
  }

  /**
   * Tests if the selectionModel is changes when the table's model
   * changes. The test shows that the selectionModel is not replaced but the
   * selection is cleared.
   *
   * @oaram harness the test harness to use
   */
  private void testSelectionModel(TestHarness harness)
  {
    harness.checkPoint("selectionModel");
    JTable table = new JTable();
    ListSelectionModel m1 = table.getSelectionModel();
    m1.addSelectionInterval(1, 1);
    harness.check(m1.isSelectedIndex(1), true);
    table.setModel(new DefaultTableModel());
    harness.check(table.getSelectionModel() == m1);
    harness.check(m1.isSelectedIndex(1), false);
  }

  /**
   * Tests if changing this property fires a property change event.
   *
   * @param harness the test harness to use
   */
  private void testPropertyFired(TestHarness harness)
  {
    harness.checkPoint("propertyFired");
    JTable table = new JTable();
    table.addPropertyChangeListener(new PropertyChangeHandler());
    DefaultTableModel m1 = new DefaultTableModel();
    DefaultTableModel m2 = new DefaultTableModel();
    propertyChangeFired = false;
    table.setModel(m1);
    harness.check(propertyChangeFired, true);
    propertyChangeFired = false;
    table.setModel(m1);
    harness.check(propertyChangeFired, false);
    propertyChangeFired = false;
    table.setModel(m2);
    harness.check(propertyChangeFired, true);
    try
      {
        table.setModel(null);
        harness.fail("IllegalArgumenException must be fired");
      }
    catch (IllegalArgumentException ex)
      {
        harness.check(true);
      }
    
  }
}
