/* remove.java -- Some checks for the remove() methods in the
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

public class remove implements Testlet
{
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
  }
  
  public void test1(TestHarness harness) 
  {
    harness.checkPoint("(int)");
    DefaultMutableTreeNode n1 = new DefaultMutableTreeNode("A");
    DefaultMutableTreeNode n2 = new DefaultMutableTreeNode("B");
    DefaultMutableTreeNode n3 = new DefaultMutableTreeNode("C");
    DefaultMutableTreeNode n4 = new DefaultMutableTreeNode("D");
    n1.add(n2);
    n1.add(n3);
    n1.add(n4);
    
    // remove valid item
    n1.remove(2);
    harness.check(n1.getChildCount(), 2);
    harness.check(n1.isNodeChild(n4), false);
    harness.check(n4.getParent(), null);
    
    // remove negative index
    boolean pass = false;
    try
    {
      n1.remove(-1);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // remove index = childCount
    pass = false;
    try
    {
      n1.remove(n1.getChildCount());
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  public void test2(TestHarness harness)
  {
    harness.checkPoint("(MutableTreeNode)");
    
    // remove known item
    DefaultMutableTreeNode n1 = new DefaultMutableTreeNode("A");
    DefaultMutableTreeNode n2 = new DefaultMutableTreeNode("B");
    DefaultMutableTreeNode n3 = new DefaultMutableTreeNode("C");
    DefaultMutableTreeNode n4 = new DefaultMutableTreeNode("D");
    n1.add(n2);
    n1.add(n3);
    n1.add(n4);
    n1.remove(n4);
    harness.check(n1.getChildCount(), 2);
    harness.check(n1.isNodeChild(n4), false);
    harness.check(n4.getParent(), null);
    
    // remove unknown item
    boolean pass = false;
    try
    {
      n1.remove(new DefaultMutableTreeNode("Z"));
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // remove null
    pass = false;
    try
    {
      n1.remove(null);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
