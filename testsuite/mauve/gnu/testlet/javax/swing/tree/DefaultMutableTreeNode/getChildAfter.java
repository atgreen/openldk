// Tags: JDK1.2
// Uses: DefaultMutableTreeNodeTest

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

public class getChildAfter implements Testlet
{
  public void test(TestHarness h)
  {
    DefaultMutableTreeNode n1 = new DefaultMutableTreeNode("A");
    DefaultMutableTreeNode n2 = new DefaultMutableTreeNode("B");
    DefaultMutableTreeNode n3 = new DefaultMutableTreeNode("C");
    DefaultMutableTreeNode n4 = new DefaultMutableTreeNode("D");
    n1.add(n2);
    n1.add(n3);
    
    h.check(n1.getChildAfter(n2), n3);
    h.check(n1.getChildAfter(n3), null);
    
    // check null argument
    boolean ok = false;
    try
      {
        n1.getChildAfter(null);
      }
    catch (IllegalArgumentException e)
      {
        ok = true;
      }
    h.check(ok);

    // check with an invalid child
    ok = false;
    try
      {
        n4.getChildAfter(n1);
      }
    catch (IllegalArgumentException e)
      {
        ok = true;
      }
    h.check(ok);
  }
}