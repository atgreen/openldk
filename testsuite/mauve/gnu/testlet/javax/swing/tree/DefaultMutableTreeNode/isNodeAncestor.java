/* isNodeAncestor.java -- Some checks for the isNodeAncestor() method in the
                          DefaultMutableTreeNode class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
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

package gnu.testlet.javax.swing.tree.DefaultMutableTreeNode;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.tree.DefaultMutableTreeNode;

public class isNodeAncestor implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("(MutableTreeNode)");
    DefaultMutableTreeNode n1 = new DefaultMutableTreeNode("A");   
    DefaultMutableTreeNode n2 = new DefaultMutableTreeNode("B");   
    DefaultMutableTreeNode n3 = new DefaultMutableTreeNode("C");   
    n1.add(n2);
    n2.add(n3);
    harness.check(n2.isNodeAncestor(n1), true);
    harness.check(n2.isNodeAncestor(n2), true);
    harness.check(n3.isNodeAncestor(n1), true);
    harness.check(n3.isNodeAncestor(n2), true);
    harness.check(n3.isNodeAncestor(n3), true);
    
    harness.check(n1.isNodeAncestor(n2), false);
    harness.check(n1.isNodeAncestor(n3), false);
    harness.check(n1.isNodeAncestor(null), false);
  }
}
