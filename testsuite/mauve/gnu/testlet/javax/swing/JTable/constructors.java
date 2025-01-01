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

import java.awt.Dimension;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import javax.swing.JTable;
import javax.swing.plaf.ColorUIResource;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;

/**
 * Some checks for the constructors in the {@link JTable} class.
 */
public class constructors
    implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    constructor1(harness);
    constructor2(harness);
    constructor3(harness);
    constructor4(harness);
    constructor5(harness);
    constructor6(harness);
    constructor7(harness);
    cellProperties(harness);
    selectionProperties(harness);
    editingProperties(harness);
    columnModelProperties(harness);
    selectionModelProperties(harness);
    linesNotNeeded(harness);
  }

  public void constructor1(TestHarness harness)
  {
    harness.checkPoint("JTable()");
    JTable table = new JTable();
    harness.check(table.getAutoCreateColumnsFromModel(), true);
  }

  public void constructor2(TestHarness harness)
  {
    harness.checkPoint("JTable(int, int)");
    JTable table = new JTable(1, 2);
    harness.check(table.getAutoCreateColumnsFromModel(), true);

    table = new JTable(0, 2);
    table = new JTable(1, 0);

    // negative rows
    boolean pass = false;
    try
      {
        /* JTable t = */new JTable(- 1, 2);
      }
    catch (IllegalArgumentException e)
      {
        pass = true;
      }
    harness.check(pass);

    // negative columns
    pass = false;
    try
      {
        /* JTable t = */new JTable(1, - 2);
      }
    catch (IllegalArgumentException e)
      {
        pass = true;
      }
    harness.check(pass);
  }

  public void constructor3(TestHarness harness)
  {
    harness.checkPoint("JTable(Object[][], Object[])");

    // try null data
    boolean pass = false;
    try
      {
        /* JTable t1 = */new JTable(null, new String[] { "AA", "BB" });
      }
    catch (NullPointerException e)
      {
        pass = true;
      }
    harness.check(pass);

    // try null column identifiers
    pass = false;
    try
      {
        /* JTable t2 = */new JTable(new String[][] { { "AA", "BB" } }, null);
      }
    catch (NullPointerException e)
      {
        pass = true;
      }
    harness.check(pass);
  }

  public void constructor4(TestHarness harness)
  {
    harness.checkPoint("JTable(TableModel)");
    JTable t = new JTable(new DefaultTableModel());
    harness.check(t.getAutoCreateColumnsFromModel(), true);

    // a null model is acceptable, it gets replaced with a default model
    t = new JTable(null);
    harness.check(t.getModel() != null);
  }

  public void constructor5(TestHarness harness)
  {
    harness.checkPoint("JTable(TableModel, TableColumnModel)");
    JTable t = new JTable(new DefaultTableModel(), null);
    harness.check(t.getAutoCreateColumnsFromModel(), true);

    t = new JTable(new DefaultTableModel(), new DefaultTableColumnModel());
    harness.check(t.getAutoCreateColumnsFromModel(), false);
  }

  public void constructor6(TestHarness harness)
  {
    harness.checkPoint("JTable(TableModel, TableColumnModel, ListSelectionModel)");
    JTable t = new JTable(new DefaultTableModel(), null, null);
    harness.check(t.getAutoCreateColumnsFromModel(), true);

    t = new JTable(new DefaultTableModel(), new DefaultTableColumnModel(), null);
    harness.check(t.getAutoCreateColumnsFromModel(), false);
  }

  public void constructor7(TestHarness harness)
  {
    harness.checkPoint("JTable(Vector, Vector)");
  }

  public void cellProperties(TestHarness harness)
  {
    harness.checkPoint("JTable row, column and cell properties.");
    JTable t = new JTable();
    harness.check(t.getAutoCreateColumnsFromModel());
    harness.check(t.getAutoResizeMode(), t.AUTO_RESIZE_SUBSEQUENT_COLUMNS);
    harness.check(t.getColumnCount(), 0);
    harness.check(t.getRowCount(), 0);
    harness.check(t.getColumnCount(), 0);
  }

  public void selectionProperties(TestHarness harness)
  {
    harness.checkPoint("JTable selection properties.");
    JTable t = new JTable();
    harness.check(t.getCellSelectionEnabled(), false);
    harness.check(t.getColumnSelectionAllowed(), false);
    harness.check(t.getRowSelectionAllowed(), true);
    harness.check(t.getSelectedColumn(), - 1);
    harness.check(t.getSelectedColumnCount(), 0);
    harness.check(t.getSelectedRow(), - 1);
    harness.check(t.getSelectedRowCount(), 0);
  }

  public void editingProperties(TestHarness harness)
  {
    harness.checkPoint("JTable visual and editing properties.");
    JTable t = new JTable();
    harness.check(t.getCellEditor(), null);
    harness.check(t.getDragEnabled(), false);
    harness.check(t.getGridColor(), new ColorUIResource(122, 138, 153));
    harness.check(t.getIntercellSpacing(), new Dimension(1, 1));
    harness.check(t.getPreferredScrollableViewportSize(), new Dimension(450,
                                                                        400));
    harness.check(t.getRowMargin(), 1);
    harness.check(t.getScrollableTracksViewportHeight(), false);
    harness.check(t.getScrollableTracksViewportWidth(), true);
    harness.check(t.getShowHorizontalLines(), true);
    harness.check(t.getShowVerticalLines(), true);
  }

  public void columnModelProperties(TestHarness harness)
  {
    harness.checkPoint("JTable columnModel properties");
    JTable t = new JTable();
    harness.check(t.getColumnModel() != null);
    harness.check(t.getColumnModel().getColumnCount(), 0);
    harness.check(t.getColumnModel().getColumnMargin(), 1);
    harness.check(t.getColumnModel().getColumnSelectionAllowed(), false);
    harness.check(t.getColumnModel().getSelectedColumnCount(), 0);
    harness.check(t.getColumnModel().getTotalColumnWidth(), 0);
    harness.check(t.getColumnModel().getSelectionModel() != null);
    harness.check(t.getColumnModel().getSelectionModel().getLeadSelectionIndex(), -1);
    harness.check(t.getColumnModel().getSelectionModel().getLeadSelectionIndex(), -1);
  }
  
  public void selectionModelProperties(TestHarness harness)
  {
    harness.checkPoint("JTable selectionModel properties");
    JTable t = new JTable();
    harness.check(t.getSelectionModel() != null);
    harness.check(t.getSelectionModel().getAnchorSelectionIndex(), -1);
    harness.check(t.getSelectionModel().getLeadSelectionIndex(), -1);
    harness.check(t.getSelectionModel().getMaxSelectionIndex(), -1);
    harness.check(t.getSelectionModel().getMinSelectionIndex(), -1);
    harness.check(t.getSelectionModel().getSelectionMode(),2);
    harness.check(t.getSelectionModel().getValueIsAdjusting(), false);
  }
  
  public void linesNotNeeded(TestHarness harness)
  {
    harness.checkPoint("4 lines not needed in constructor");
    JTable t = new JTable();
    harness.check(t.getSelectionModel().getAnchorSelectionIndex(), -1);
    harness.check(t.getSelectionModel().getLeadSelectionIndex(), -1);
    harness.check(t.getColumnModel().getSelectionModel().getAnchorSelectionIndex(), -1);
    harness.check(t.getColumnModel().getSelectionModel().getLeadSelectionIndex(), -1);
    t.getSelectionModel().setAnchorSelectionIndex(-1);
    t.getSelectionModel().setLeadSelectionIndex(-1);
    t.getColumnModel().getSelectionModel().setAnchorSelectionIndex(-1);
    t.getColumnModel().getSelectionModel().setLeadSelectionIndex(-1);
    // Same values as before.
    harness.check(t.getSelectionModel().getAnchorSelectionIndex(), -1);
    harness.check(t.getSelectionModel().getLeadSelectionIndex(), -1);
    harness.check(t.getColumnModel().getSelectionModel().getAnchorSelectionIndex(), -1);
    harness.check(t.getColumnModel().getSelectionModel().getLeadSelectionIndex(), -1);
  }
}
