/* insert.java -- Some checks for the insert() method in the
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

public class insert implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("(MutableTreeNode, int)");
    DefaultMutableTreeNode n1 = new DefaultMutableTreeNode("A");
    DefaultMutableTreeNode n2 = new DefaultMutableTreeNode("B");
    DefaultMutableTreeNode n3 = new DefaultMutableTreeNode("C");
    DefaultMutableTreeNode n4 = new DefaultMutableTreeNode("D");
    n1.insert(n2, 0);
    harness.check(n1.getChildAt(0), n2);
    n1.insert(n3, 0);
    harness.check(n1.getChildAt(0), n3);
    harness.check(n1.getChildAt(1), n2);
    n1.insert(n4, 2);
    harness.check(n1.getChildAt(0), n3);
    harness.check(n1.getChildAt(1), n2);
    harness.check(n1.getChildAt(2), n4);
    
    // null 
    boolean pass = false;
    try
      {
        n1.insert(null, 0);
      }
    catch (IllegalArgumentException e)
      {
        pass = true;
      }
    harness.check(pass);
    
    // negative
    pass = false;
    DefaultMutableTreeNode n5 = new DefaultMutableTreeNode("E");
    try
      {
        n1.insert(n5, -1);
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        pass = true;  
      }
    harness.check(pass);
    
    // count + 1
    pass = false;
    try
      {
        n1.insert(n5, 4);
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        pass = true;  
      }
    harness.check(pass);
    
    // node that doesn't allow children
    DefaultMutableTreeNode n6 = new DefaultMutableTreeNode("F", false);
    DefaultMutableTreeNode n7 = new DefaultMutableTreeNode("G");
    pass = false;
    try
      {
        n6.insert(n7, 0);
      }
    catch (IllegalStateException e)
      {
        pass = true;  
      }
    harness.check(pass);
    
    // node that is an ancestor
    DefaultMutableTreeNode n8 = new DefaultMutableTreeNode("H");
    DefaultMutableTreeNode n9 = new DefaultMutableTreeNode("I");
    DefaultMutableTreeNode n10 = new DefaultMutableTreeNode("J");
    n8.add(n9);
    n9.add(n10);
    pass = false;
    try
      {
        n10.insert(n8, 0);
      }
    catch (IllegalArgumentException e)
      {
        pass = true;  
      }
    harness.check(pass);
  }
}
