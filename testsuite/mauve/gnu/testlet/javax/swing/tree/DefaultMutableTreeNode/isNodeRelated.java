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

import java.util.Enumeration;
import java.util.Vector;

import javax.swing.tree.*;

public class isNodeRelated implements Testlet
{
  public void test(TestHarness h)
  {
    DefaultMutableTreeNode node = new DefaultMutableTreeNode();

    h.check(! node.isNodeRelated(null));
    h.check(! node.isNodeRelated(DefaultMutableTreeNodeTest.A));
    
    Vector nodes = DefaultMutableTreeNodeTest.nodes;
    
    for (int i = 0; i < nodes.size(); ++i)
      for (int j = 0; j < nodes.size(); ++j)
        {
          DefaultMutableTreeNode node1 = (DefaultMutableTreeNode) nodes.get(i);
          DefaultMutableTreeNode node2 = (DefaultMutableTreeNode) nodes.get(j);

          h.check(node1.isNodeRelated(node2), "isNodeRelated");
        }

  }
}