/* isRowSelected.java -- Tests JTree.isRowSelected()
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

package gnu.testlet.javax.swing.JTree;

import javax.swing.JTree;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreePath;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests JTree.isRowSelected().
 */
public class isRowSelected implements Testlet
{

  /**
   * Indicates if JTree.getRowForPath() is called during a test.
   */
  boolean getPathForRowCalled;

  /**
   * Indicates if JTree.isPathSelected() is called during a test.
   */
  boolean isPathSelectedCalled;

  /**
   * Indicates if a tree's selection model isRowSelected() is called
   * during a test.
   */
  boolean modelIsRowSelectedCalled;

  /**
   * A subclass of JTree for testing.
   */
  private class TestTree
    extends JTree
  {
    public TestTree(Object[] data)
    {
      super(data);
    }
    public TreePath getPathForRow(int r)
    {
      getPathForRowCalled = true;
      return super.getPathForRow(r);
    }
    public boolean isPathSelected(TreePath p)
    {
      isPathSelectedCalled = true;
      return super.isPathSelected(p);
    }
  }

  /**
   * A subclass of DefaultTreeSelectionModel for testing.
   */
  private class TestTreeSelectionModel
    extends DefaultTreeSelectionModel
  {
    public boolean isRowSelected(int r)
    {
      modelIsRowSelectedCalled = true;
      return super.isRowSelected(r);
    }
  }

  /**
   * Entry point into test suite.
   */
  public void test(TestHarness harness)
  {
    testCallGetPathForRow(harness);
    testCallIsPathSelected(harness);
    testCallModelIsRowSelected(harness);
  }

  /**
   * Tests if isRowSelected should call JTree.getRowForPath(), or if it
   * should leave the row->path mapping to the selection model.
   *
   * @param h the test harness
   */
  private void testCallGetPathForRow(TestHarness h)
  {
    h.checkPoint("testCallGetPathForRow");
    Object[] data = new Object[]{ "Hello", "World" };
    TestTree t = new TestTree(data);
    getPathForRowCalled = false;
    t.isRowSelected(0);
    h.check(getPathForRowCalled, false);
  }

  /**
   * Tests if isRowSelected should call JTree.isPathSelected, or if it
   * should leave the row->path mapping to the selection model.
   *
   * @param h the test harness
   */
  private void testCallIsPathSelected(TestHarness h)
  {
    h.checkPoint("testCallIsPathSelected");
    Object[] data = new Object[]{ "Hello", "World" };
    TestTree t = new TestTree(data);
    isPathSelectedCalled = false;
    t.isRowSelected(0);
    h.check(isPathSelectedCalled, false);
  }

  /**
   * Tests if JTree.isRowSelected() should call the model's isRowSelected()
   * method.
   *
   * @param h the test harness
   */
  private void testCallModelIsRowSelected(TestHarness h)
  {
    h.checkPoint("testCallGetRowForPath");
    Object[] data = new Object[]{ "Hello", "World" };
    TestTree t = new TestTree(data);
    t.setSelectionModel(new TestTreeSelectionModel());
    modelIsRowSelectedCalled = false;
    t.isRowSelected(0);
    h.check(modelIsRowSelectedCalled, true);
  }
}
