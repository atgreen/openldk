// Tags: not-a-test

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
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.util.List;

import gnu.testlet.TestHarness;

import java.util.List;

/**
 * A utility class that performs various checks on the subList() method in the 
 * {@link List} interface, for an arbitrary list class.
 */
public class subList
{
  /**
   * Creates a new instance of the specified list class.
   * 
   * @param listClass  the class.
   * @param harness  the harness.
   * 
   * @return A new list instance.
   */
  static List createListInstance(Class listClass, TestHarness harness) 
  {
    List result = null;
    try
    {
      result = (List) listClass.newInstance();
      return result;
    }
    catch (Exception e)
    {
      harness.debug(e);   
    }
    return null;
  }
  
  /**
   * Run all tests for a particular class of list.
   * 
   * @param listClass  the list class.
   * @param harness  the test harness.
   */
  public static void testAll(Class listClass, TestHarness harness) 
  {
    testEmptyList(listClass, harness); 
    testABCD(listClass, harness); 
    testAdd(listClass, harness); 
    testClear(listClass, harness); 
    testRemove(listClass, harness);
    testSet(listClass, harness);
    testSubSubList(listClass, harness);
  }
  
  public static void testEmptyList(Class listClass, TestHarness harness) 
  {
    // create a sublist in an empty list...
    List list = createListInstance(listClass, harness);
    List sub = list.subList(0, 0);
    harness.check(sub.isEmpty());
  }
  
  public static void testABCD(Class listClass, TestHarness harness)
  {
    List list = createListInstance(listClass, harness);
    list.add("A");
    list.add("B");
    list.add("C");
    list.add("D");
    
    // check start index == end index
    List sub = list.subList(2, 2);
    harness.check(sub.isEmpty());
    
    // sublist with 1 item
    sub = list.subList(1, 2);
    harness.check(sub.get(0).equals("B"));
    harness.check(sub.size(), 1);
    
    // sublist with end index == size is OK
    sub = list.subList(1, 4);
    harness.check(sub.size(), 3);
    sub = list.subList(4, 4);
    harness.check(sub.isEmpty());
    
    // sublist with start index > end index
    // see also bug report 4506427, which details the exceptions thrown
    boolean pass = false;
    try
    {
      sub = list.subList(2, 1);
    }
    catch (IndexOutOfBoundsException e)
    {
      pass = true;   
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;  
    }
    harness.check(pass);
    
    // sublist with negative start index
    pass = false;
    try
    {
      sub = list.subList(-1, 1);
    }
    catch (IndexOutOfBoundsException e)
    {
      pass = true;   
    }
    harness.check(pass);

    // sublist with end index > size
    pass = false;
    try
    {
      sub = list.subList(1, 5);
    }
    catch (IndexOutOfBoundsException e)
    {
      pass = true;   
    }
    harness.check(pass);
  }
 
  public static void testAdd(Class listClass, TestHarness harness)
  {
    List list = createListInstance(listClass, harness);
    list.add("A");
    list.add("B");
    list.add("C");
    list.add("D");
    
    // add to an empty list
    List sub = list.subList(0, 0);
    sub.add("1");
    harness.check(list.get(0).equals("1"));
    harness.check(list.size(), 5);
    
    // add one item via sublist
    sub = list.subList(1, 2);
    sub.add("2");
    harness.check(list.get(0).equals("1"));
    harness.check(list.get(1).equals("A"));
    harness.check(list.get(2).equals("2"));
    harness.check(list.get(3).equals("B"));
    harness.check(list.size(), 6);    
  }

  public static void testClear(Class listClass, TestHarness harness)
  {
    List list = createListInstance(listClass, harness);
    list.add("A");
    list.add("B");
    list.add("C");
    list.add("D");
    
    // clearing an empty sublist should not affect original
    List sub = list.subList(0, 0);
    sub.clear();
    harness.check(list.size(), 4);
    
    // clear one item via sublist
    sub = list.subList(1, 2);
    sub.clear();
    harness.check(list.get(0).equals("A"));
    harness.check(list.get(1).equals("C"));
    harness.check(list.size(), 3);
    
    // clear all
    sub = list.subList(0, list.size());
    sub.clear();
    harness.check(list.isEmpty());
  }

  public static void testRemove(Class listClass, TestHarness harness)
  {
    List list = createListInstance(listClass, harness);
    list.add("A");
    list.add("B");
    list.add("C");
    list.add("D");
    
    // clear one item via sublist
    List sub = list.subList(1, 2);
    sub.remove("B");
    harness.check(list.get(0).equals("A"));
    harness.check(list.get(1).equals("C"));
    harness.check(list.size(), 3);
  }
  
  public static void testSet(Class listClass, TestHarness harness)
  {
    List list = createListInstance(listClass, harness);
    list.add("A");
    list.add("B");
    list.add("C");
    list.add("D");
    
    // clear one item via sublist
    List sub = list.subList(1, 2);
    sub.set(0, "X");
    harness.check(list.get(0).equals("A"));
    harness.check(list.get(1).equals("X"));
    harness.check(list.get(2).equals("C"));
    harness.check(list.size(), 4);
  }
  
  public static void testSubSubList(Class listClass, TestHarness harness) 
  {
    List list = createListInstance(listClass, harness);
    list.add("A");
    list.add("B");
    list.add("C");
    list.add("D");
    
    // clear one item via sublist
    List sub1 = list.subList(0, 4);  
    List sub2 = sub1.subList(1, 3);
    sub2.add("X");
    
    harness.check(sub1.get(1).equals("B"));
    harness.check(sub1.get(2).equals("C"));
    harness.check(sub1.get(3).equals("X"));

    harness.check(list.get(0).equals("A"));
    harness.check(list.get(1).equals("B"));
    harness.check(list.get(2).equals("C"));
    harness.check(list.get(3).equals("X"));
    harness.check(list.get(4).equals("D"));
  }
  
}
