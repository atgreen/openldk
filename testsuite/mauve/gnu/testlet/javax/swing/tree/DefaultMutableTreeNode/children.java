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

import javax.swing.tree.*;

public class children implements Testlet
{
  public void test(TestHarness h)
  {
    Enumeration e;

    e = DefaultMutableTreeNodeTest.A.children();

    DefaultMutableTreeNodeTest.checkEnumeration(h, e, DefaultMutableTreeNodeTest.B);
    DefaultMutableTreeNodeTest.checkEnumeration(h, e, DefaultMutableTreeNodeTest.C);
    DefaultMutableTreeNodeTest.checkEnumeration(h, e, DefaultMutableTreeNodeTest.D);
    DefaultMutableTreeNodeTest.checkEnumeration(h, e, null);

    e = DefaultMutableTreeNodeTest.G.children();
    DefaultMutableTreeNodeTest.checkEnumeration(h, e, DefaultMutableTreeNodeTest.L);
    DefaultMutableTreeNodeTest.checkEnumeration(h, e, DefaultMutableTreeNodeTest.M);
    DefaultMutableTreeNodeTest.checkEnumeration(h, e, null);

    e = DefaultMutableTreeNodeTest.K.children();
    DefaultMutableTreeNodeTest.checkEnumeration(h, e, null);
    
    e = DefaultMutableTreeNodeTest.L.children();
    DefaultMutableTreeNodeTest.checkEnumeration(h, e, DefaultMutableTreeNodeTest.S);
    DefaultMutableTreeNodeTest.checkEnumeration(h, e, null);
    
    e = DefaultMutableTreeNodeTest.Z.children();
    DefaultMutableTreeNodeTest.checkEnumeration(h, e, null);
  }
}