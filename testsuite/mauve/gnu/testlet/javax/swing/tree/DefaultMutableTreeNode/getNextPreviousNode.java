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

public class getNextPreviousNode implements Testlet
{
  public void test(TestHarness h)
  {
    h.check(DefaultMutableTreeNodeTest.A.getPreviousNode(), null);
    h.check(DefaultMutableTreeNodeTest.A.getNextNode(), DefaultMutableTreeNodeTest.B);
    
    h.check(DefaultMutableTreeNodeTest.Q.getPreviousNode(), DefaultMutableTreeNodeTest.J);
    h.check(DefaultMutableTreeNodeTest.Q.getNextNode(), DefaultMutableTreeNodeTest.R);
    
    h.check(DefaultMutableTreeNodeTest.P.getPreviousNode(), DefaultMutableTreeNodeTest.V);
    h.check(DefaultMutableTreeNodeTest.P.getNextNode(), null);
    
    h.check(DefaultMutableTreeNodeTest.Z.getPreviousNode(), DefaultMutableTreeNodeTest.Y);
    h.check(DefaultMutableTreeNodeTest.Z.getNextNode(), DefaultMutableTreeNodeTest.V);
    
    h.check(DefaultMutableTreeNodeTest.X.getPreviousNode(), DefaultMutableTreeNodeTest.W);
    h.check(DefaultMutableTreeNodeTest.X.getNextNode(), DefaultMutableTreeNodeTest.M);
    
    h.check(DefaultMutableTreeNodeTest.G.getPreviousNode(), DefaultMutableTreeNodeTest.C);
    h.check(DefaultMutableTreeNodeTest.G.getNextNode(), DefaultMutableTreeNodeTest.L);
  }
}