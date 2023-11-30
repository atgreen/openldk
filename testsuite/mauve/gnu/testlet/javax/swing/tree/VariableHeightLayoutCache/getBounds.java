/* getBounds.java -- Tests VariableHeightLayoutCache.getBounds()
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

package gnu.testlet.javax.swing.tree.VariableHeightLayoutCache;

import java.awt.Rectangle;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.VariableHeightLayoutCache;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks the VariableHeightLayoutCache.getBounds() method.
 *
 */
public class getBounds implements Testlet
{

  /**
   * The entry point for the test.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testNullRect(harness);
  }

  /**
   * Checks how null arguments for the rectangle is handle.
   *
   * @param h the test harness to use
   */
  private void testNullRect(TestHarness h)
  {
    VariableHeightLayoutCache c = new VariableHeightLayoutCache();
    DefaultMutableTreeNode n = new DefaultMutableTreeNode();
    TreePath p = new TreePath(n);
    DefaultTreeModel m = new DefaultTreeModel(n);
    c.setModel(m);
    Rectangle r = new Rectangle();
    // Check if the supplied rectangle is reused.
    Rectangle s = c.getBounds(p, r);
    h.check(r == s);
    // Check if a new rectangle is created when supplying null. 
    s = c.getBounds(p, null);
    h.check(s != null);
    h.check(s != r);
  }
}
