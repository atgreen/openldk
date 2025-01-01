/* removeAllChildren.java -- some checks for the removeAllChildren() method in
                             the DefaultMutableTreeNode class.
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

public class removeAllChildren implements Testlet 
{
  public void test(TestHarness harness)
  {
    DefaultMutableTreeNode n1 = new DefaultMutableTreeNode("A");
    // calling when there are no children should do nothing
    n1.removeAllChildren();
    harness.check(n1.getChildCount(), 0);
    
    DefaultMutableTreeNode n2 = new DefaultMutableTreeNode("B");
    DefaultMutableTreeNode n3 = new DefaultMutableTreeNode("C");
    n1.add(n2);
    n1.add(n3);
    n1.removeAllChildren();
    harness.check(n1.getChildCount(), 0);
    harness.check(n2.getParent(), null);
    harness.check(n3.getParent(), null);    
  }
}