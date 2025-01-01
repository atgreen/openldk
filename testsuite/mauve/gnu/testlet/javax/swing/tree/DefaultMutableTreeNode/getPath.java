// Tags: JDK1.2
// Uses: DefaultMutableTreeNodeTest

// Copyright (C) 2004  Michael Koch <konqueror@gmx.de>

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

package gnu.testlet.javax.swing.tree.DefaultMutableTreeNode;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.tree.*;

public class getPath implements Testlet
{
  public void test(TestHarness h)
  {
    TreeNode[] path;

    path = DefaultMutableTreeNodeTest.A.getPath();

    h.check(path.length, 1, "array length 1");
    h.check(path[0], DefaultMutableTreeNodeTest.A, "correct tree node 1");

    path = DefaultMutableTreeNodeTest.U.getPath();

    h.check(path.length, 5, "array length 2");
    h.check(path[0], DefaultMutableTreeNodeTest.A, "correct tree node 2");
    h.check(path[1], DefaultMutableTreeNodeTest.D, "correct tree node 3");
    h.check(path[2], DefaultMutableTreeNodeTest.I, "correct tree node 4");
    h.check(path[3], DefaultMutableTreeNodeTest.O, "correct tree node 5");
    h.check(path[4], DefaultMutableTreeNodeTest.U, "correct tree node 6");
  }
}