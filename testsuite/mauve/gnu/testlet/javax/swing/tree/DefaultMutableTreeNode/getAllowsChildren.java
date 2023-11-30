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

public class getAllowsChildren implements Testlet
{
  public void test(TestHarness h)
  {
    h.check(DefaultMutableTreeNodeTest.A.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.B.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.C.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.D.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.E.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.F.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.G.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.H.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.I.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.J.getAllowsChildren());
    h.check(! DefaultMutableTreeNodeTest.K.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.L.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.M.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.N.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.O.getAllowsChildren());
    h.check(! DefaultMutableTreeNodeTest.P.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.Q.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.R.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.S.getAllowsChildren());
    h.check(! DefaultMutableTreeNodeTest.T.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.U.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.V.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.W.getAllowsChildren());
    h.check(! DefaultMutableTreeNodeTest.X.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.Y.getAllowsChildren());
    h.check(DefaultMutableTreeNodeTest.Z.getAllowsChildren());
  }
}