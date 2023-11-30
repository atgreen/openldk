/* sort.java -- some checks for the sort() methods in the Collections class.
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

package gnu.testlet.java.util.Collections;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public class sort implements Testlet {

  public void test(TestHarness harness)
  {
    testMethod1(harness);
    testMethod2(harness);
  }
  
  public void testMethod1(TestHarness harness)
  {
    harness.checkPoint("(List)");
    List list = new ArrayList();
    
    // sort an empty list - presumably all that can go wrong is that the code
    // throws some exception due to a coding error...
    boolean pass = true;
    try
    {
      Collections.sort(list); 
    }
    catch (Exception e)
    {
      pass = false;
    }
    harness.check(pass);
    
    // sort a list containing just one item
    list = new ArrayList();
    list.add("A");
    Collections.sort(list);
    harness.check(list.size(), 1);
    harness.check(list.get(0), "A");
    
    // sort a list containing two items
    list = new ArrayList();
    list.add("B");
    list.add("A");
    Collections.sort(list);
    harness.check(list.size(), 2);
    harness.check(list.get(0), "A");
    harness.check(list.get(1), "B");
    
    // sort a list containing three items
    list = new ArrayList();
    list.add("B");
    list.add("A");
    list.add("C");
    Collections.sort(list);
    harness.check(list.size(), 3);
    harness.check(list.get(0), "A");
    harness.check(list.get(1), "B");
    harness.check(list.get(2), "C");
    
    // sort a list with a null in it
    pass = false;
    try
    {
      list = new ArrayList();
      list.add("B");
      list.add("A");
      list.add(null);
      Collections.sort(list);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // check that equal items don't change order
    Object obj1 = new Integer(9500);
    Object obj2 = new Integer(9600);
    Object obj3 = new Integer(9500);
    Object obj4 = new Integer(9600);
    list = new ArrayList();
    list.add(obj1);
    list.add(obj2);
    list.add(obj3);
    list.add(obj4);
    Collections.sort(list);
    harness.check(list.size(), 4);
    harness.check(list.get(0), obj1);
    harness.check(list.get(1), obj3);
    harness.check(list.get(2), obj2);
    harness.check(list.get(3), obj4);
    
    // try a null argument
    pass = false;
    try
    {
      Collections.sort(null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  static class MyComparator implements Comparator
  {

    public int compare(Object obj0, Object obj1) {
        Comparable c0 = (Comparable) obj0;
        Comparable c1 = (Comparable) obj1;
        return -c0.compareTo(c1);
    }
      
  }
  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(List, Comparator)");
    List list = new ArrayList();
    Comparator comparator = new MyComparator();
    
    // sort an empty list - presumably all that can go wrong is that the code
    // throws some exception due to a coding error...
    boolean pass = true;
    try
    {
      Collections.sort(list, comparator); 
    }
    catch (Exception e)
    {
      pass = false;
    }
    harness.check(pass);
    
    // sort a list containing just one item
    list = new ArrayList();
    list.add("A");
    Collections.sort(list, comparator);
    harness.check(list.size(), 1);
    harness.check(list.get(0), "A");
    
    // sort a list containing two items
    list = new ArrayList();
    list.add("B");
    list.add("A");
    Collections.sort(list, comparator);
    harness.check(list.size(), 2);
    harness.check(list.get(0), "B");
    harness.check(list.get(1), "A");
    
    // sort a list containing three items
    list = new ArrayList();
    list.add("B");
    list.add("A");
    list.add("C");
    Collections.sort(list, comparator);
    harness.check(list.size(), 3);
    harness.check(list.get(0), "C");
    harness.check(list.get(1), "B");
    harness.check(list.get(2), "A");
    
    // sort a list with a null in it
    pass = false;
    try
    {
      list = new ArrayList();
      list.add("B");
      list.add("A");
      list.add(null);
      Collections.sort(list, comparator);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);

    // check that equal items don't change order
    Object obj1 = new Integer(9500);
    Object obj2 = new Integer(9600);
    Object obj3 = new Integer(9500);
    Object obj4 = new Integer(9600);
    list = new ArrayList();
    list.add(obj1);
    list.add(obj2);
    list.add(obj3);
    list.add(obj4);
    Collections.sort(list, comparator);
    harness.check(list.size(), 4);
    harness.check(list.get(0), obj2);
    harness.check(list.get(1), obj4);
    harness.check(list.get(2), obj1);
    harness.check(list.get(3), obj3);
    
    // try a null argument 1
    pass = false;
    try
    {
      Collections.sort(null, comparator);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);

    // try a null argument 2
    pass = true;
    try
    {
      Collections.sort(new ArrayList(), null);
    }
    catch (NullPointerException e)
    {
      pass = false;
    }
    harness.check(pass);
  
  }

}