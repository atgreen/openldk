/* getTableCellRendererComponent.java -- Tests the getTableCellRenderer() method
   Copyright (C) 2006 Roman Kennke (kennke@aicas.com)
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.table.DefaultTableCellRenderer;

import java.awt.Component;

import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.table.DefaultTableCellRenderer;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests various aspects of the getTableCellRendererComponent() method in
 * DefaultTableCellRenderer.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class getTableCellRendererComponent implements Testlet
{

  Object value;

  /**
   * Overridden to test if setValue should be called.
   */
  class TestCellRenderer extends DefaultTableCellRenderer
  {
    protected void setValue(Object v)
    {
      super.setValue(v);
      value = v;
    }
  }

  /**
   * The entry point into this test.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testSetValue(harness);
    testTextField(harness);
  }

  /**
   * Checks if getTableCellRendererComponent() should call setValue()
   * in order to set the value.
   *
   * @param h the test harness to use
   */
  private void testSetValue(TestHarness h)
  {
    TestCellRenderer r = new TestCellRenderer();
    value = null;
    JTable t = new JTable();
    Object v = "Hello";
    r.getTableCellRendererComponent(t, v, false, false, 0, 0);
    h.check(value == v);
  }

  /**
   * Checks how values of type JTextFields should be handled. This test
   * is here due to a programming mistake in Classpath's implementation
   * of DefaultTableCellRenderer.
   *
   * @param h the test harness to use
   */
  private void testTextField(TestHarness h)
  {
    TestCellRenderer r = new TestCellRenderer();
    JTable t = new JTable();
    Object v = new JTextField("Hello");
    Component c = r.getTableCellRendererComponent(t, v, false, false, 0, 0);
    h.check(c == r);
  }
}
