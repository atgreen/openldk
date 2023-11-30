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

public class add implements Testlet
{
  public void test(TestHarness h)
  {
    h.checkPoint("(MutableTreeNode)");
    DefaultMutableTreeNode a, b, c;

    a = new DefaultMutableTreeNode();
    b = new DefaultMutableTreeNode();
    c = new DefaultMutableTreeNode(null, false);

    // try adding null
    boolean ok = false;
    try
      {
        a.add(null);
      }
    catch (IllegalArgumentException e)
      {
        ok = true;
      }
    h.check(ok);

    // try adding to a node that doesn't allow children
    ok = false;
    try
      {
        c.add(a);
      }
    catch (IllegalStateException e)
      {
        ok = true;
      }
    h.check(ok);

    a.add(b);
    a.add(c);
    
    h.check(a.isNodeChild(b));
    h.check(b.getParent(), a);
    h.check(a.isNodeChild(c));
    h.check(c.getParent(), a);
    h.check(!b.isNodeChild(c));
    
    b.add(c);

    h.check(a.isNodeChild(b));
    h.check(!a.isNodeChild(c));
    h.check(b.isNodeChild(c));
    
    // can we add a node to itself? no because a node is considered its own
    // ancestor
    boolean pass = false;
    try
    {
      a.add(a);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    h.check(pass);
    
    // same goes for adding a as a child of c
    pass = false;
    try
    {
      b.add(a);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    h.check(pass);
    
  }
}