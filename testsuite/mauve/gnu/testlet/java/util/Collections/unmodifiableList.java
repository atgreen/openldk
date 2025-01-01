// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

// This file is part of Mauve.

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

package gnu.testlet.java.util.Collections;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Collections;
import java.util.List;

/**
 * Some checks for the unmodifiableList() method in the Collections class.
 */
public class unmodifiableList implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness.
   */
  public void test(TestHarness harness) 
  {
    // test an empty list
    harness.checkPoint("Empty List");
    List list1 = new java.util.ArrayList();
    testList(list1, harness);
    
    // test a non-empty list
    harness.checkPoint("Non-empty List");
    List list2 = new java.util.ArrayList();
    list2.add("A");
    list2.add("B");
    list2.add("C");
    testList(list2, harness);
    
    // try a null list - the spec says that the argument should be non-null
    // but doesn't say what exception will be thrown if it is null (assuming
    // NullPointerException)...
    harness.checkPoint("Null List");
    boolean pass = false;
    try
    {
      Collections.unmodifiableList(null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);

  }
  
  /**
   * Runs the test using the specified harness.
   * 
   * @param list  the list to test
   * @param harness  the test harness.
   */
  private void testList(List list, TestHarness harness) 
  {
    List ulist = Collections.unmodifiableList(list);
    boolean pass = false;
    try
    {
      ulist.add("X");   
    }
    catch (UnsupportedOperationException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      ulist.add(0, "X");   
    }
    catch (UnsupportedOperationException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    List otherList = new java.util.ArrayList();
    otherList.add("Z");
    pass = false;
    try
    {
      ulist.addAll(otherList);   
    }
    catch (UnsupportedOperationException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      ulist.clear();   
    }
    catch (UnsupportedOperationException e) 
    {
      pass = true;   
    }
    harness.check(pass);

    pass = false;
    try
    {
      ulist.remove("X");   
    }
    catch (UnsupportedOperationException e) 
    {
      pass = true;   
    }
    harness.check(pass);

    pass = false;
    try
    {
      ulist.remove(0);   
    }
    catch (UnsupportedOperationException e) 
    {
      pass = true;   
    }
    harness.check(pass);
  
    pass = false;
    try
    {
      ulist.removeAll(otherList);   
    }
    catch (UnsupportedOperationException e) 
    {
      pass = true;   
    }
    harness.check(pass);

    pass = false;
    try
    {
      ulist.retainAll(otherList);   
    }
    catch (UnsupportedOperationException e) 
    {
      pass = true;   
    }
    harness.check(pass);
  
    pass = false;
    try
    {
      ulist.set(0, "X");   
    }
    catch (UnsupportedOperationException e) 
    {
      pass = true;   
    }
    harness.check(pass);
  }
  
}
