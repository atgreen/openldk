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

public class getChildCount implements Testlet
{
  public void test(TestHarness h)
  {
    h.check(DefaultMutableTreeNodeTest.A.getChildCount(), 3);
    h.check(DefaultMutableTreeNodeTest.B.getChildCount(), 2);
    h.check(DefaultMutableTreeNodeTest.C.getChildCount(), 1);
    h.check(DefaultMutableTreeNodeTest.D.getChildCount(), 2);
    h.check(DefaultMutableTreeNodeTest.E.getChildCount(), 1);
    h.check(DefaultMutableTreeNodeTest.F.getChildCount(), 1);
    h.check(DefaultMutableTreeNodeTest.G.getChildCount(), 2);
    h.check(DefaultMutableTreeNodeTest.H.getChildCount(), 1);
    h.check(DefaultMutableTreeNodeTest.I.getChildCount(), 2);
    h.check(DefaultMutableTreeNodeTest.J.getChildCount(), 2);
    h.check(DefaultMutableTreeNodeTest.K.getChildCount(), 0);
    h.check(DefaultMutableTreeNodeTest.L.getChildCount(), 1);
    h.check(DefaultMutableTreeNodeTest.M.getChildCount(), 1);
    h.check(DefaultMutableTreeNodeTest.N.getChildCount(), 0);
    h.check(DefaultMutableTreeNodeTest.O.getChildCount(), 2);
    h.check(DefaultMutableTreeNodeTest.P.getChildCount(), 0);
    h.check(DefaultMutableTreeNodeTest.Q.getChildCount(), 0);
    h.check(DefaultMutableTreeNodeTest.R.getChildCount(), 0);
    h.check(DefaultMutableTreeNodeTest.S.getChildCount(), 2);
    h.check(DefaultMutableTreeNodeTest.T.getChildCount(), 0);
    h.check(DefaultMutableTreeNodeTest.U.getChildCount(), 1);
    h.check(DefaultMutableTreeNodeTest.V.getChildCount(), 0);
    h.check(DefaultMutableTreeNodeTest.W.getChildCount(), 0);
    h.check(DefaultMutableTreeNodeTest.X.getChildCount(), 0);
    h.check(DefaultMutableTreeNodeTest.Y.getChildCount(), 1);
    h.check(DefaultMutableTreeNodeTest.Z.getChildCount(), 0);
  }
}