/* SubListTest.java -- Test for subList.
   Copyright (C) 2007 Mario Torre <neugens@liamasoftware.net> 
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

// Tags: JDK1.5

package gnu.testlet.java.util.concurrent.CopyOnWriteArrayList;

import java.util.ConcurrentModificationException;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * @author Mario Torre <neugens@liamasoftware.net> 
 */
public class SubListTest implements Testlet
{
  private int [] data = {
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19                    
  };
  
  public void test(TestHarness harness)
  {
    CopyOnWriteArrayList<Integer> list =
      new CopyOnWriteArrayList<Integer>();
    
    for (Integer element : data)
      list.add(element);
    
    List<Integer> subList = list.subList(5, 15);
    int i = 5;
    for (int element : subList)
      {
        harness.check(element == i);
        i++;
      }
    
    // remove all the element from the subList and check the elements in the
    // list
    harness.checkPoint("clear list");
    
    subList.clear();
    
    harness.check(subList.size() == 0);
    harness.check(list.size() == 10);
    
    i = 0;
    for (int element : list)
      {
        harness.check(element == i);
        i++;
        if (i > 4 && i < 15)
          i = 15;
      }
    
    harness.checkPoint("ConcurrentModificationException");
    
    list.clear();
    for (Integer element : data)
      list.add(element);
    
    subList = list.subList(5, 15);
    list.remove(5);
    
    try
      {
        for (int element : subList)
          {
            // we should never get here
            harness.check(false);
          }
      }
    catch (ConcurrentModificationException e)
      {
        harness.check(true);
      }
    
    harness.checkPoint("Remove elements from SubList");
    
    list.clear();
    for (Integer element : data)
      list.add(element);
    
    subList = list.subList(5, 15);
    
    subList.remove(0);
    subList.remove(0);
    
    harness.check(subList.size() == 8);
    harness.check(list.size() == 18);
    
    subList.add(0, 6);
    subList.add(0, 5);
    i = 5;
    for (int element : subList)
      {
        harness.check(element == i);
        i++;
      }
    
  }
}
