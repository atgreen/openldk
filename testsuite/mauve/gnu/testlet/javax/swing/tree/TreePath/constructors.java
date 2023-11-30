// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.javax.swing.tree.TreePath;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Arrays;

import javax.swing.tree.TreePath;

/**
 * Some tests for the constructors in the {@link TreePath} class.
 */
public class constructors implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
    testConstructor5(harness);
  }

  private void testConstructor1(TestHarness harness) 
  {
    harness.checkPoint("TreePath()");     
    // this constructor is protected, so need to subclass to access it    
  }
  
  private void testConstructor2(TestHarness harness) 
  {
    harness.checkPoint("TreePath(Object)"); 
    TreePath p = new TreePath("XYZ");
    harness.check(p.getPathCount(), 1);
    harness.check(p.getLastPathComponent(), "XYZ");
    harness.check(p.getParentPath(), null);
    harness.check(Arrays.equals(p.getPath(), new String[] { "XYZ" }));
    
    // null argument 
    boolean pass = false;
    try
    {
      /*TreePath p2 =*/ new TreePath(null);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  private void testConstructor3(TestHarness harness) 
  {
    harness.checkPoint("TreePath(Object[])");        
    
    // null argument 
    boolean pass = false;
    try
    {
      /*TreePath p2 =*/ new TreePath((Object[]) null);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  private void testConstructor4(TestHarness harness) 
  {
    harness.checkPoint("TreePath(Object[], int)");            
  }
  
  private void testConstructor5(TestHarness harness) 
  {
    harness.checkPoint("TreePath(TreePath, Object)");      
  }

}
