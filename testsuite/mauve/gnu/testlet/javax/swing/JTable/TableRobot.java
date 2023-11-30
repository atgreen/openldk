/* TableRobot.java -- JTable test
 Copyright (C) 2006 Audrius Meskauskas
 This file is part of Mauve.

 Mauve is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2, or (at your option)
 any later version.

 Mauve is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with Mauve; see the file COPYING.  If not, write to the
 Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 02110-1301 USA.

 */

// Tags: JDK1.4 GUI


package gnu.testlet.javax.swing.JTable;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.org.omg.CORBA.Asserter;

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.BitSet;
import java.util.Random;
import java.util.TreeSet;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableModel;

/**
 * The AWT robot based test for JTable.
 */
public class TableRobot extends Asserter implements Testlet
{
  /**
   * The number of columns in the table (fixed).
   */
  int columns = 3;

  /**
   * The number of characters in each field (all the same).
   */
  int sizes = 4;

  /**
   * The table being tested.
   */
  JTable table = new JTable(0, 3);

  JFrame frame;

  /**
   * The table row with the empty cells.
   */
  String[] EMPTY;

  Robot r;

  static Random ran = new Random();

  String[][] records = new String[5][];

  int rows = 0;

  /**
   * Get the record, containing the random information.
   * 
   * @return the record, containing the random information
   */
  String[] getRandomRecord()
  {

    String[] r = new String[columns];

    for (int i = 0; i < r.length; i++)
      {
        char[] c = new char[sizes];
        for (int j = 0; j < c.length; j++)
          {
            // Random valid ASCII chars from 0 till 0
            c[j] = (char) ('0' + ran.nextInt('9' - '0'));
          }

        r[i] = new String(c);
      }
    return r;
  }

  public String[] createRecord()
  {
    String[] record = getRandomRecord();
    ((DefaultTableModel) table.getModel()).addRow(EMPTY);

    for (int i = 0; i < record.length; i++)
      {
        // Click on the cell:
        clickCell(table, table.getRowCount() - 1, i);
        
        // Press F2 to start the editing session:
        r.keyPress(KeyEvent.VK_F2);
        r.keyRelease(KeyEvent.VK_F2);        
        for (int j = 0; j < record[i].length(); j++)
          {
            typeDigit(record[i].charAt(j));
          }
        r.keyPress(KeyEvent.VK_ENTER);
        r.keyRelease(KeyEvent.VK_ENTER);
      }

    return record;
  }

  public void verifyTableContent(String[][] records)
  {
    // Verify content.
    for (int row = 0; row < records.length; row++)
      {
        for (int column = 0; column < records[row].length; column++)
          {
            if (!table.getValueAt(row, column).equals(records[row][column]))
              {
                String msg = "Match failed " + row + ":" + column + ", exp "
                             + records[row][column] + " act "
                             + table.getValueAt(row, column);
                fail(msg);
              }
          }
      }
  }

  public String[][] getTableContent(int columns)
  {
    String[][] records = new String[table.getRowCount()][];

    for (int row = 0; row < records.length; row++)
      {
        records[row] = new String[columns];
        for (int col = 0; col < columns; col++)
          {
            records[row][col] = table.getModel().getValueAt(row, col).toString();
          }
      }
    return records;
  }

  public String toString(String[] record)
  {
    StringBuffer b = new StringBuffer();
    for (int i = 0; i < record.length; i++)
      {
        b.append(record[i]);
        b.append("#");
      }
    return b.toString();
  }

  public void testTable() throws Exception
  {
    vCreateEditing();
    testMultipleSelection();
    vEditing();
    vTestArrowKeyNavigation();    
    vDeleteOneByOne();
  }

  private void vDeleteOneByOne()
  {
    // Delete remaining records one by one.
    for (int i = 0; i < records.length; i++)
      {

        assertEquals("Deleting row should reduce the row count", records.length
                                                                 - i,
                     table.getRowCount());
        // Verify the first line.
        for (int column = 0; column < records[i].length; column++)
          {
            assertEquals(i + "/" + records.length, table.getValueAt(0, column),
                         records[i][column]);
          }

        // Select the first record.
        clickCell(table, 0, 0);

        // Delete the first line.
        ((DefaultTableModel) table.getModel()).removeRow(table.getSelectedRow());
      }
  }

  private void vEditing()
  {
    // Test editing.
    TreeSet used = new TreeSet();
    for (int i = 0; i < 7; i++)
      {

        int row;
        int col;
        String img;
        do
          {
            row = ran.nextInt(records.length);
            col = ran.nextInt(3);
            img = row + ":" + col;
          }
        while (used.contains(img));

        used.add(img);

        String nc = Integer.toString(ran.nextInt(10));
        clickCell(table, row, col);

        // Press F2 to start the editing session:
        r.keyPress(KeyEvent.VK_F2);
        r.keyRelease(KeyEvent.VK_F2);        
        
        String prev = table.getModel().getValueAt(row, col).toString();

        int code = KeyEvent.VK_0 + (nc.charAt(0) - '0');

        r.keyPress(code);
        r.keyRelease(code);

        r.keyPress(KeyEvent.VK_ENTER);
        r.keyRelease(KeyEvent.VK_ENTER);        

        records[row][col] = records[row][col] + nc;
        assertEquals("Incorrect value after editing",
                     table.getModel().getValueAt(row, col), records[row][col]);
      }
  }

  private void testMultipleSelection()
  {
    // Randomly select half of the records and delete them in one action:
    int nd = records.length / 2;

    BitSet selected = new BitSet();

    // Hold the ctrl down after the first click.
    boolean ctrl = false;
    for (int i = 0; i < nd; i++)
      {
        int d;
        do
          {
            d = ran.nextInt(records.length);
          }
        while (selected.get(d));

        // Select the first record.
        clickCell(table, d, 0);
        selected.set(d);
        if (!ctrl)
          {
            r.keyPress(KeyEvent.VK_CONTROL);
            ctrl = true;
          }
      }
    // Release the ctrl key:
    r.keyRelease(KeyEvent.VK_CONTROL);

    // Verify the selected rows.

    // Compare selected record count:
    int[] srows = table.getSelectedRows();

    assertEquals("New row should increase the length", nd, srows.length);

    for (int i = 0; i < srows.length; i++)
      {
        assertEquals("Selection mismatch", table.isRowSelected(srows[i]),
                     selected.get(srows[i]));
      }
  }

  private void vCreateEditing()
  {
    // Create the last several rows by editing either.
    for (int i = 0; i < records.length; i++)
      {
        assertEquals("New row should increase the length", rows++,
                     table.getRowCount());
        records[i] = createRecord();
      }

    verifyTableContent(records);
  }
  
  private void vTestArrowKeyNavigation()
  {
    clickCell(table, 0,0);
    assertEquals("First click", table.getSelectedRow(), 0);
    
    for (int i = 1; i < table.getRowCount(); i++)
      {
        r.keyPress(KeyEvent.VK_DOWN);
        r.keyRelease(KeyEvent.VK_DOWN);
        assertEquals("VK_DOWN", table.getSelectedRow(), i);
      }
    
    for (int i = table.getRowCount()-2; i >=0; i--)
      {
        r.keyPress(KeyEvent.VK_UP);
        r.keyRelease(KeyEvent.VK_UP);
        assertEquals("VK_UP", table.getSelectedRow(), i);
      }
    
    for (int i = 1; i < table.getColumnCount(); i++)
      {
        r.keyPress(KeyEvent.VK_RIGHT);
        r.keyRelease(KeyEvent.VK_RIGHT);
        assertEquals("VK_DOWN", table.getSelectedColumn(), i);
      }
    
    for (int i = table.getColumnCount()-2; i >=0; i--)
      {
        r.keyPress(KeyEvent.VK_LEFT);
        r.keyRelease(KeyEvent.VK_LEFT);
        assertEquals("VK_RIGHT", table.getSelectedColumn(), i);
      }
    
    // Test multiple row selection with shift.
    r.keyPress(KeyEvent.VK_SHIFT);
    
    for (int i = 1; i < table.getRowCount(); i++)
      {
        r.keyPress(KeyEvent.VK_DOWN);
        r.keyRelease(KeyEvent.VK_DOWN);
        assertEquals("VK_DOWN+shift", table.getSelectedRowCount(), i+1);
      }
    
    for (int i = table.getRowCount()-2; i >=0; i--)
      {
        r.keyPress(KeyEvent.VK_UP);
        r.keyRelease(KeyEvent.VK_UP);
        assertEquals("VK_UP+shift", table.getSelectedRowCount(), i+1);
      }
    
    // Test multiple row selection with shift.
    r.keyRelease(KeyEvent.VK_SHIFT);    
  }

  protected void setUp() throws Exception
  {
    EMPTY = new String[columns];
    for (int i = 0; i < EMPTY.length; i++)
      {
        EMPTY[i] = "";
      }

    frame = new JFrame();
    frame.getContentPane().add(table);
    frame.setSize(400, 400);
    frame.setVisible(true);

    r = new Robot();
    r.setAutoDelay(50);
    r.delay(500);
  }

  protected void tearDown() throws Exception
  {
    frame.dispose();
  }

  /**
   * Click the mouse on center of the given component.
   */
  public void click(JComponent c)
  {
    Point p = new Point();
    p.x = c.getWidth() / 2;
    p.y = c.getHeight() / 2;

    SwingUtilities.convertPointToScreen(p, c);
    r.mouseMove(p.x, p.y);
    r.mousePress(InputEvent.BUTTON1_MASK);
    r.mouseRelease(InputEvent.BUTTON1_MASK);
  }

  /**
   * Click the given main databas view cell.
   * 
   * @param row
   *          the row
   * @param column
   *          the column
   */
  public void clickCell(JTable table, int row, int column)
  {
    Rectangle rect = table.getCellRect(row, column, true);
    Point p = new Point(rect.x + rect.width / 2, rect.y + rect.height / 2);
    SwingUtilities.convertPointToScreen(p, table);
    r.mouseMove(p.x, p.y);
    r.mousePress(InputEvent.BUTTON1_MASK);
    r.delay(50);
    r.mouseRelease(InputEvent.BUTTON1_MASK);
    r.delay(50);
  }

  public void typeDigit(char character)
  {
    int code = KeyEvent.VK_0 + (character - '0');
    r.keyPress(code);
    r.keyRelease(code);
  }

  public void test(TestHarness harness)
  {
    try
      {
        h = harness;
        setUp();
        try
          {
            testTable();
          }
        finally
          {
            tearDown();
          }
      }
    catch (Exception e)
      {
        e.printStackTrace();
        h.fail("Exception: " + e);
      }
  }

}
