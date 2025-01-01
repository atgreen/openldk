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

package gnu.testlet.javax.swing.table.TableColumn;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.DefaultCellEditor;
import javax.swing.JCheckBox;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

/**
 * Some tests for the constructors in the {@link TableColumn} class.
 */
public class constructors implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
  }

  private void testConstructor1(TestHarness harness) 
  {
    harness.checkPoint("TableColumn()");
    TableColumn c1 = new TableColumn();
    harness.check(c1.getModelIndex(), 0);
    harness.check(c1.getMinWidth(), 15);
    harness.check(c1.getWidth(), 75);
    harness.check(c1.getMaxWidth(), Integer.MAX_VALUE);
    harness.check(c1.getCellRenderer(), null);
    harness.check(c1.getCellEditor(), null);
    harness.check(c1.getHeaderValue(), null);
    harness.check(c1.getResizable(), true);
  }

  private void testConstructor2(TestHarness harness)  
  {
    harness.checkPoint("TableColumn(int)");
    TableColumn c1 = new TableColumn(1);
    harness.check(c1.getModelIndex(), 1);
    harness.check(c1.getMinWidth(), 15);
    harness.check(c1.getWidth(), 75);
    harness.check(c1.getMaxWidth(), Integer.MAX_VALUE);
    harness.check(c1.getCellRenderer(), null);
    harness.check(c1.getCellEditor(), null);
    harness.check(c1.getHeaderValue(), null);
    harness.check(c1.getResizable(), true);
    
    TableColumn c2 = new TableColumn(-1);
    harness.check(c2.getModelIndex(), -1);
  }
  
  private void testConstructor3(TestHarness harness) 
  {
    harness.checkPoint("TableColumn(int, int)"); 
    TableColumn c1 = new TableColumn(1, 33);
    harness.check(c1.getModelIndex(), 1);
    harness.check(c1.getMinWidth(), 15);
    harness.check(c1.getWidth(), 33);
    harness.check(c1.getMaxWidth(), Integer.MAX_VALUE);
    harness.check(c1.getCellRenderer(), null);
    harness.check(c1.getCellEditor(), null);
    harness.check(c1.getHeaderValue(), null);
    harness.check(c1.getResizable(), true);
    
    // negative width
    TableColumn c2 = new TableColumn(1, -1);
    harness.check(c2.getWidth(), -1);
  }
  
  private void testConstructor4(TestHarness harness)  
  {
    harness.checkPoint("TableColumn(int, int, TableCellRenderer, TableCellEditor)");
    TableCellRenderer renderer = new DefaultTableCellRenderer();
    TableCellEditor editor = new DefaultCellEditor(new JCheckBox());
    TableColumn c1 = new TableColumn(1, 33, renderer, editor);
    harness.check(c1.getModelIndex(), 1);
    harness.check(c1.getMinWidth(), 15);
    harness.check(c1.getWidth(), 33);
    harness.check(c1.getMaxWidth(), Integer.MAX_VALUE);
    harness.check(c1.getCellRenderer(), renderer);
    harness.check(c1.getCellEditor(), editor);
    harness.check(c1.getHeaderValue(), null);
    harness.check(c1.getResizable(), true);
  }

}
