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

package gnu.testlet.javax.swing.event.TableModelEvent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.event.TableModelEvent;
import javax.swing.table.DefaultTableModel;

/**
 * Some checks for the constructors in the {@link TableModelEvent} class.
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
    testConstructor5(harness);
  }

  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("(TableModel)");
    DefaultTableModel tm = new DefaultTableModel(2, 3);
    TableModelEvent event = new TableModelEvent(tm);
    harness.check(event.getSource() == tm);
    harness.check(event.getType(), TableModelEvent.UPDATE);
    harness.check(event.getColumn(), TableModelEvent.ALL_COLUMNS);
    harness.check(event.getFirstRow(), 0);
    harness.check(event.getLastRow(), Integer.MAX_VALUE);
    
    // try a null argument...
    boolean pass = false;
    try
    {
      /*TableModelEvent e =*/ new TableModelEvent(null);    
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    catch (NullPointerException e) {
      pass = false;   
    }
    harness.check(pass);
  }

  public void testConstructor2(TestHarness harness) 
  {
    harness.checkPoint("(TableModel, int)"); 
    DefaultTableModel tm = new DefaultTableModel(2, 3);
    TableModelEvent event = new TableModelEvent(tm, 1);
    harness.check(event.getSource() == tm);
    harness.check(event.getType(), TableModelEvent.UPDATE);
    harness.check(event.getColumn(), TableModelEvent.ALL_COLUMNS);
    harness.check(event.getFirstRow(), 1);
    harness.check(event.getLastRow(), 1);
    
    // try a null argument...
    boolean pass = false;
    try
    {
      /*TableModelEvent e =*/ new TableModelEvent(null, 1);    
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
  }

  public void testConstructor3(TestHarness harness) 
  {
    harness.checkPoint("(TableModel, int, int)"); 
    DefaultTableModel tm = new DefaultTableModel(2, 3);
    TableModelEvent event = new TableModelEvent(tm, 0, 2);
    harness.check(event.getSource() == tm);
    harness.check(event.getType(), TableModelEvent.UPDATE);
    harness.check(event.getColumn(), TableModelEvent.ALL_COLUMNS);
    harness.check(event.getFirstRow(), 0);
    harness.check(event.getLastRow(), 2);
    
    // try a null argument...
    boolean pass = false;
    try
    {
      /*TableModelEvent e =*/ new TableModelEvent(null, 1, 1);    
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try first row > lastRow
    pass = false;
    TableModelEvent e = new TableModelEvent(tm, 2, 0);
    harness.check(e.getFirstRow(), 2);
    harness.check(e.getLastRow(), 0);
  }

  public void testConstructor4(TestHarness harness) 
  {
    harness.checkPoint("(TableModel, int, int, int)"); 
    DefaultTableModel tm = new DefaultTableModel(2, 3);
    TableModelEvent event = new TableModelEvent(tm, 0, 2, 1);
    harness.check(event.getSource() == tm);
    harness.check(event.getType(), TableModelEvent.UPDATE);
    harness.check(event.getColumn(), 1);
    harness.check(event.getFirstRow(), 0);
    harness.check(event.getLastRow(), 2);
    
    // try a null argument...
    boolean pass = false;
    try
    {
      /*TableModelEvent e =*/ new TableModelEvent(null, 1, 1, 0);    
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
  }

  public void testConstructor5(TestHarness harness) 
  {
    harness.checkPoint("(TableModel, int, int, int, int)"); 
    DefaultTableModel tm = new DefaultTableModel(2, 3);
    TableModelEvent event = new TableModelEvent(tm, 0, 2, 1, 
            TableModelEvent.DELETE);
    harness.check(event.getSource() == tm);
    harness.check(event.getType(), TableModelEvent.DELETE);
    harness.check(event.getColumn(), 1);
    harness.check(event.getFirstRow(), 0);
    harness.check(event.getLastRow(), 2);
    
    // try a null argument...
    boolean pass = false;
    try
    {
      /*TableModelEvent e =*/ new TableModelEvent(null, 1, 1, 0, 
            TableModelEvent.INSERT);    
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try an unknown type
    TableModelEvent e = new TableModelEvent(tm, 0, 2, 1, 999);
    harness.check(e.getType(), 999);
    
  }
}

