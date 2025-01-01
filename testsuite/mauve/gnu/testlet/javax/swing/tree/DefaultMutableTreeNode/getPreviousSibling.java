/* getPreviousSibling.java -- some checks for the getPreviousSibling() method 
                              in the DefaultMutableTreeNode class.
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

public class getPreviousSibling implements Testlet 
{
  public void test(TestHarness harness)
  {
    DefaultMutableTreeNode n1 = new DefaultMutableTreeNode("A");
    harness.check(n1.getPreviousSibling(), null);
    DefaultMutableTreeNode n2 = new DefaultMutableTreeNode("B");
    DefaultMutableTreeNode n3 = new DefaultMutableTreeNode("C");
    n1.add(n2);
    n1.add(n3);
    harness.check(n2.getPreviousSibling(), null);
    harness.check(n3.getPreviousSibling(), n2);   
  }
}
