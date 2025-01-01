/* isAddedPath.java -- some checks for the isAddedPath() methods in the
                       TreeSelectionEvent class.
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

// Tags: JDK1.3

package gnu.testlet.javax.swing.event.TreeSelectionEvent;

import javax.swing.event.TreeSelectionEvent;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class isAddedPath implements Testlet 
{
  public void test(TestHarness harness)
  {
    checkMethod1(harness);   
    checkMethod2(harness);
    checkMethod3(harness);
  }
  
  public void checkMethod1(TestHarness harness)
  {
    harness.checkPoint("()");
    TreeSelectionModel m = new DefaultTreeSelectionModel();
    TreePath p1 = new TreePath("A");
    TreeSelectionEvent tse = new TreeSelectionEvent(m, p1, false, null, null);
    harness.check(tse.isAddedPath(), false);
  }

  public void checkMethod2(TestHarness harness)
  {
    harness.checkPoint("(int)");
    TreeSelectionModel m = new DefaultTreeSelectionModel();
    TreePath p1A = new TreePath("A");
    TreePath p1B = new TreePath("AA");
    TreePath[] p1 = new TreePath[] {p1A, p1B};
    boolean[] b = new boolean[] {true, false};
    TreeSelectionEvent tse = new TreeSelectionEvent(m, p1, b, null, null);
    harness.check(tse.isAddedPath(), true);
    harness.check(tse.isAddedPath(0), true);
    harness.check(tse.isAddedPath(1), false);
  }

  public void checkMethod3(TestHarness harness)
  {
    harness.checkPoint("(TreePath)");
    TreeSelectionModel m = new DefaultTreeSelectionModel();
    TreePath p1A = new TreePath("A");
    TreePath p1B = new TreePath("AA");
    TreePath[] p1 = new TreePath[] {p1A, p1B};
    boolean[] b = new boolean[] {true, false};
    TreeSelectionEvent tse = new TreeSelectionEvent(m, p1, b, null, null);
    harness.check(tse.isAddedPath(p1A), true);
    harness.check(tse.isAddedPath(p1B), false);
    
    // try path not recognised
    boolean pass = false;
    try
    {
      tse.isAddedPath(new TreePath("X"));
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try null path
    pass = false;
    try
    {
      tse.isAddedPath(null);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
