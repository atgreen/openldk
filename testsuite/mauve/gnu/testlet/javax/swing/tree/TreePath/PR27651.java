/* PR27651.java -- a check related to PR27651
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

package gnu.testlet.javax.swing.tree.TreePath;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.tree.TreePath;

public class PR27651 implements Testlet
{
  static class A
  {
    String id;
    
    public A(String id)
    {
      if (id == null)
        id = "";
      this.id = id;
    }
    
    public boolean equals(Object obj)
    {
      if (obj == this)
        return true;
      if (obj instanceof A)
        {
          A that = (A) obj;
          return this.id.equals(that.id);
        }
      return false;
    }
  }
  
  static class B extends A
  {
    int index;
    
    public B(String id, int index)
    {
      super(id);
      this.index = index;
    }

    /**
     * Throws a ClassCastException if obj is not an instanceof B.
     */
    public boolean equals(Object obj)
    {
      if (obj == this)
        return true;
      if (! super.equals(obj))
        return false;
      B that = (B) obj;
      return (this.index == that.index);
    } 
    
  }

  /**
   * This checks an implementation detail - that the reference implementation
   * calls objA.equals(objB) within the TreePath.isDescendant() method, and
   * not the reverse.
   */
  public void test(TestHarness harness)
  {
    A objA = new A("ABC");
    B objB = new B("ABC", 99);
    
    // confirm that objA.equals(objB) is true, but the reverse throws a 
    // ClassCastException...
    harness.check(objA.equals(objB));
    boolean pass = false;
    try
      {
        objB.equals(objA);
      }
    catch (ClassCastException e)
      {
        pass = true;
      }
    harness.check(pass);
    
    TreePath p1 = new TreePath(objA);
    TreePath p2 = new TreePath(objB);
    harness.check(p1.isDescendant(p2));
    
    pass = false;
    try
      {
        p2.isDescendant(p1);
      }
    catch (ClassCastException e)
      {
        pass = true;    
      }
    harness.check(pass);
  }
}
