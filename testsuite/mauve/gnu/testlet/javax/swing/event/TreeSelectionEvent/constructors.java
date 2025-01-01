/* constructors.java -- some checks for the constructors in the 
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.event.TreeSelectionEvent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.event.TreeSelectionEvent;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

public class constructors implements Testlet
{
  public void test(TestHarness harness)
  {
    checkConstructor1(harness);
    checkConstructor2(harness);
  }
  
  public void checkConstructor1(TestHarness harness)
  {
    harness.checkPoint("(Object, TreePath, boolean, TreePath, TreePath)");
    TreeSelectionModel m = new DefaultTreeSelectionModel();
    TreePath p1 = new TreePath("A");
    TreePath p2 = new TreePath("B");
    TreePath p3 = new TreePath("C");
    TreeSelectionEvent tse = new TreeSelectionEvent(m, p1, true, p2, p3);
    harness.check(tse.getSource(), m);
    harness.check(tse.getPath(), p1);
    harness.check(tse.getPaths().length, 1);
    harness.check(tse.getPaths()[0], p1);
    harness.check(tse.isAddedPath());
    harness.check(tse.getOldLeadSelectionPath(), p2);
    harness.check(tse.getNewLeadSelectionPath(), p3);
    
    // check null source
    boolean pass = false;
    try
    {
      tse = new TreeSelectionEvent(null, p1, true, p2, p3);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // check null path
    tse = new TreeSelectionEvent(m, null, true, p2, p3);
    harness.check(tse.getPath(), null);
    harness.check(tse.getPaths().length, 1);
    harness.check(tse.getPaths()[0], null);
    
    // check null old path
    tse = new TreeSelectionEvent(m, p1, true, null,  p3);
    harness.check(tse.getOldLeadSelectionPath(), null);
    
    // check null new path
    tse = new TreeSelectionEvent(m, p1, true, p2, null);
    harness.check(tse.getNewLeadSelectionPath(), null);
  }

  public void checkConstructor2(TestHarness harness)
  {
    harness.checkPoint("(Object, TreePath[], boolean[], TreePath, TreePath)");
    TreeSelectionModel m = new DefaultTreeSelectionModel();
    TreePath p1A = new TreePath("A");
    TreePath p1B = new TreePath("AA");
    TreePath[] p1 = new TreePath[] {p1A, p1B};
    TreePath p2 = new TreePath("B");
    TreePath p3 = new TreePath("C");
    boolean[] b = new boolean[] {true, false};
    TreeSelectionEvent tse = new TreeSelectionEvent(m, p1, b, p2, p3);
    harness.check(tse.getSource(), m);
    harness.check(tse.getPath(), p1A);
    harness.check(tse.getPaths().length, 2);
    harness.check(tse.getPaths()[0], p1A);
    harness.check(tse.getPaths()[1], p1B);
    harness.check(tse.isAddedPath(), true);
    harness.check(tse.isAddedPath(0), true);
    harness.check(tse.isAddedPath(1), false);
    harness.check(tse.getOldLeadSelectionPath(), p2);
    harness.check(tse.getNewLeadSelectionPath(), p3);
    
    // check null source
    boolean pass = false;
    try
    {
      tse = new TreeSelectionEvent(null, p1, b, p2, p3);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // check null path array
    tse = new TreeSelectionEvent(m, null, b, p2, p3);
    
    // ...constructor allows it, but then fails at getPath()
    pass = false;
    try
      {
        tse.getPath();
      }
    catch (NullPointerException e)
      {
        pass = true;
      }
    harness.check(pass);
    pass = false;
    try
    {
      tse.getPaths();
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
        
    // check null boolean array
    tse = new TreeSelectionEvent(m, p1, null, p2, p3);
    pass = false;
    try
      {
        tse.isAddedPath();
      }
    catch (NullPointerException e)
      {
        pass = true;
      }
    harness.check(pass);
    
    pass = false;
    try
      {
        tse.isAddedPath(0);
      }
    catch (NullPointerException e)
      {
        pass = true;  
      }
    harness.check(pass);
    
    // check null old path
    tse = new TreeSelectionEvent(m, p1, b, null,  p3);
    harness.check(tse.getOldLeadSelectionPath(), null);
    
    // check null new path
    tse = new TreeSelectionEvent(m, p1, b, p2, null);
    harness.check(tse.getNewLeadSelectionPath(), null);
  }

}
