/* cloneWithSource.java -- some checks for the cloneWithSource() method in the
                           TreeSelectionEvent.
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

package gnu.testlet.javax.swing.event.TreeSelectionEvent;

import javax.swing.event.TreeSelectionEvent;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class cloneWithSource implements Testlet 
{
  public void test(TestHarness harness)
  {
    TreeSelectionModel m1 = new DefaultTreeSelectionModel();
    TreeSelectionModel m2 = new DefaultTreeSelectionModel();
    TreePath p1 = new TreePath("A");
    TreePath p2 = new TreePath("B");
    TreePath p3 = new TreePath("C");
    TreeSelectionEvent tse1 = new TreeSelectionEvent(m1, p1, true, p2, p3);
    TreeSelectionEvent tse2 = (TreeSelectionEvent) tse1.cloneWithSource(m2);
    harness.check(tse2.getSource(), m2);
    harness.check(tse2.getPath(), p1);
    harness.check(tse2.getPaths().length, 1);
    harness.check(tse2.getPaths()[0], p1);
    harness.check(tse2.isAddedPath());
    harness.check(tse2.getOldLeadSelectionPath(), p2);
    harness.check(tse2.getNewLeadSelectionPath(), p3);
    
    // try null source
    boolean pass = false;
    try
    {
      tse1.cloneWithSource(null);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
  }
}
