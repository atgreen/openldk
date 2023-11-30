// Tags: JDK1.2

// Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Vector;

public class binarySearch implements Testlet
{

  public void test(TestHarness harness) 
  {
    // run tests on ArrayList
    harness.checkPoint("ArrayList");
    genericTest(new ArrayList(), harness);
    
    // run tests on LinkedList
    harness.checkPoint("LinkedList");
    genericTest(new LinkedList(), harness);
    
    // run tests on Vector
    harness.checkPoint("Vector");
    genericTest(new Vector(), harness);
    
    // test for a known bug (10447)
    harness.checkPoint("10447");
    testBug10447(harness);

    // comparison order
    harness.checkPoint("Compare Order");
    testCompareOrder(new ArrayList(), harness);
    testCompareOrder(new LinkedList(), harness);
  }

  private void testCompareOrder(List list, TestHarness harness)
  {
    final boolean[] result = new boolean[] { false, false };
    list.add(new Comparable() {
        public int compareTo(Object obj) {
            result[0] = true;
            return -1;
        }
    });
    Collections.binarySearch(list, new Comparable() {
        public int compareTo(Object obj) {
            result[1] = true;
            return -1;
        }
    });
    harness.check(result[0] && !result[1]);

    final Object obj1 = new Object();
    final Object obj2 = new Object();
    list.clear();
    list.add(obj1);
    result[0] = false;
    Collections.binarySearch(list, obj2, new Comparator() {
        public int compare(Object o1, Object o2) {
           result[0] = (o1 == obj1 && o2 == obj2);
           return -1;
        }
    });
    harness.check(result[0]);
  }
  
  private void genericTest(List list, TestHarness harness) 
  {
    // search an empty list...
    list.clear();
    int index = Collections.binarySearch(list, "A");
    harness.check(index, -1);
    
    // search a list with one item...
    list.add("B");
    index = Collections.binarySearch(list, "B");
    harness.check(index, 0);
    index = Collections.binarySearch(list, "A");  // item that would go before "B"
    harness.check(index, -1);
    index = Collections.binarySearch(list, "C");  // item that would go after "B"
    harness.check(index, -2);
    
    // search a list with two items...
    list.add("D");
    index = Collections.binarySearch(list, "A");
    harness.check(index, -1);
    index = Collections.binarySearch(list, "B");
    harness.check(index, 0);
    index = Collections.binarySearch(list, "C");
    harness.check(index, -2);
    index = Collections.binarySearch(list, "D");  
    harness.check(index, 1);
    index = Collections.binarySearch(list, "E");  
    harness.check(index, -3);
    
    // search a list with three items...
    list.add("F");
    index = Collections.binarySearch(list, "A");
    harness.check(index, -1);
    index = Collections.binarySearch(list, "B");
    harness.check(index, 0);
    index = Collections.binarySearch(list, "C");
    harness.check(index, -2);
    index = Collections.binarySearch(list, "D");  
    harness.check(index, 1);
    index = Collections.binarySearch(list, "E");  
    harness.check(index, -3);
    index = Collections.binarySearch(list, "F");  
    harness.check(index, 2);
    index = Collections.binarySearch(list, "G");  
    harness.check(index, -4);
    
    // search some larger lists
    fillList(list, 1024);
    index = Collections.binarySearch(list, "00000");  
    harness.check(index, 0);
    index = Collections.binarySearch(list, "00123");  
    harness.check(index, 123);
    index = Collections.binarySearch(list, "00511");  
    harness.check(index, 511);
    index = Collections.binarySearch(list, "00512");  
    harness.check(index, 512);
    index = Collections.binarySearch(list, "00513");  
    harness.check(index, 513);
    index = Collections.binarySearch(list, "00789");  
    harness.check(index, 789);
    index = Collections.binarySearch(list, "01023");  
    harness.check(index, 1023);
    index = Collections.binarySearch(list, "01024");  
    harness.check(index, -1025);

    fillList(list, 12345);
    index = Collections.binarySearch(list, "00000");  
    harness.check(index, 0);
    index = Collections.binarySearch(list, "00123");  
    harness.check(index, 123);
    index = Collections.binarySearch(list, "00511");  
    harness.check(index, 511);
    index = Collections.binarySearch(list, "00512");  
    harness.check(index, 512);
    index = Collections.binarySearch(list, "00513");  
    harness.check(index, 513);
    index = Collections.binarySearch(list, "00789");  
    harness.check(index, 789);
    index = Collections.binarySearch(list, "01023");  
    harness.check(index, 1023);
    index = Collections.binarySearch(list, "12345");  
    harness.check(index, -12346);

  }

  private void fillList(List list, int itemCount) 
  {
    list.clear();
    for (int i = 0; i < itemCount; i++)
    {
      String s = String.valueOf(i);
      list.add("00000".substring(s.length()) + s);
    }
  }
  
  /**
   * A test for bug report 10447.
   * 
   * @param harness  the test harness.
   */
  private void testBug10447(TestHarness harness) 
  {
    List list = new LinkedList();
    list.add("A"); list.add("B"); list.add("C"); list.add("D"); 
    list.add("E"); list.add("F"); list.add("G"); list.add("H"); 
    list.add("I"); list.add("J"); list.add("K"); list.add("L"); 
    list.add("M"); list.add("N"); list.add("O"); list.add("P"); 
    
    // this works
    int i = Collections.binarySearch(list, "E");
    harness.check(i, 4);
    
    // this doesn't (bug seems to need at least 17 items to trigger)    
    list.add("Q"); 
    i = Collections.binarySearch(list, "E");
    harness.check(i, 4);
      
    // but all is fine for ArrayList
    List list2 = new ArrayList();
    list2.add("A"); list2.add("B"); list2.add("C"); list2.add("D"); 
    list2.add("E"); list2.add("F"); list2.add("G"); list2.add("H"); 
    list2.add("I"); list2.add("J"); list2.add("K"); list2.add("L"); 
    list2.add("M"); list2.add("N"); list2.add("O"); list2.add("P"); 
    
    // this works
    i = Collections.binarySearch(list2, "E");
    harness.check(i, 4);
    
    // and this does too   
    list2.add("Q"); 
    i = Collections.binarySearch(list2, "E");
    harness.check(i, 4);
  }
  
}
