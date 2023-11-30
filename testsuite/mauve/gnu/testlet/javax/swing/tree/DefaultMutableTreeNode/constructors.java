// Tags: JDK1.2

// Copyright (C) 2004, 2006,  Michael Koch <konqueror@gmx.de>

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.tree.DefaultMutableTreeNode;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.tree.DefaultMutableTreeNode;

public class constructors implements Testlet
{

  public void test(TestHarness h)
  {
    testConstructor1(h);
    testConstructor2(h);
    testConstructor3(h);
  }
  
  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("()");
    DefaultMutableTreeNode n = new DefaultMutableTreeNode();
    harness.check(n.getUserObject(), null);
    harness.check(n.getAllowsChildren(), true);
    harness.check(n.getLevel(), 0);
    harness.check(n.getChildCount(), 0);
    harness.check(n.children(), DefaultMutableTreeNode.EMPTY_ENUMERATION);
    harness.check(n.getDepth(), 0);
  }
  
  public void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(Object)");
    DefaultMutableTreeNode n = new DefaultMutableTreeNode("ABC");
    harness.check(n.getUserObject(), "ABC");
    harness.check(n.getAllowsChildren(), true);
    harness.check(n.getLevel(), 0);
    harness.check(n.getChildCount(), 0);
    harness.check(n.children(), DefaultMutableTreeNode.EMPTY_ENUMERATION);
    harness.check(n.getDepth(), 0);

    // try null argument
    n = new DefaultMutableTreeNode(null);
    harness.check(n.getUserObject(), null);
  }

  public void testConstructor3(TestHarness harness)
  {
    harness.checkPoint("(Object, boolean)");
    DefaultMutableTreeNode n = new DefaultMutableTreeNode("ABC", false);
    harness.check(n.getUserObject(), "ABC");
    harness.check(n.getAllowsChildren(), false);
    harness.check(n.getLevel(), 0);
    harness.check(n.getChildCount(), 0);
    harness.check(n.children(), DefaultMutableTreeNode.EMPTY_ENUMERATION);
    harness.check(n.getDepth(), 0);

    // try null argument
    n = new DefaultMutableTreeNode(null, true);
    harness.check(n.getUserObject(), null);
    harness.check(n.getAllowsChildren(), true);
  }

}