/* TestIterators.java -- test for the Iterator and ListIterator methods.
   Copyright (C) 2007 Mario Torre <neugens@limasoftware.net>
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.concurrent.CopyOnWriteArrayList;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class TestIterators implements Testlet
{
  public void test(TestHarness harness)
  {
    iteratorTests(harness);
    listIteratorTests(harness);
  }

  private void listIteratorTests(TestHarness harness)
  {
    harness.checkPoint("listIterator");
    
    int [] expected =
    { 
     0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 5, 6, 7, 8, 9
    };
    
    CopyOnWriteArrayList<Integer> list =
      new CopyOnWriteArrayList<Integer>();
    
    java.util.List<Integer> data = new ArrayList<Integer>();
    
    for (int i = 0; i < 10; i++)
      data.add(i);
    
    // list.add copy the storage array each time is called, adding elements
    // that way we avoid all this copying
    list.addAll(data);
    
    ListIterator<Integer> iterator = list.listIterator();
    int i = 0;

    harness.checkPoint("listIterator - forward");
    
    while (iterator.hasNext())
      harness.check(iterator.next().intValue() == expected[i++]);
      
    harness.checkPoint("listIterator - backward");
    
    while (iterator.hasPrevious())
      harness.check(iterator.previous().intValue() == expected[--i]);
    
    harness.checkPoint("listIterator - forward from element");
    
    iterator = list.listIterator(5);
    i = 5;
    
    while (iterator.hasNext())
      harness.check(iterator.next().intValue() == expected[i++]);
    
    harness.checkPoint("listIterator - backward from element");
    
    while (iterator.hasPrevious())
      harness.check(iterator.previous().intValue() == expected[--i]);
  }

  private void iteratorTests(TestHarness harness)
  {
    harness.checkPoint("iterator");
    
    int [] expected =
    { 
     0, 1, 2, 3, 4, 5, 6, 7, 8, 9
    };
    
    CopyOnWriteArrayList<Integer> list =
      new CopyOnWriteArrayList<Integer>();
    
    java.util.List<Integer> data = new ArrayList<Integer>();
    
    for (int i = 0; i < 10; i++)
      data.add(i);
    
    // list.add copy the storage array each time is called, adding elements
    // that way we avoid all this copying
    list.addAll(data);
    
    int i = 0;
    for (Iterator<Integer> iterator = list.iterator(); iterator.hasNext(); )
      {
        harness.check(iterator.next().intValue() == expected[i++]);
      }
    
    harness.checkPoint("iterator - snapshot");
    Iterator<Integer> iterator = list.iterator();
    
    list.clear();
    
    harness.check(list.size() == 0);
    
    // the iterator contains a snapshot of the list so resetting the list
    // has no effect to the content of the iterator.
    
    i = 0;
    while (iterator.hasNext())
      harness.check(iterator.next().intValue() == expected[i++]);
    
    harness.checkPoint("iterator - remove");
    
    list.addAll(data);
    
    try
      {
        for (Iterator<Integer> iter = list.iterator(); iter.hasNext(); )
          {
            iter.remove();
            harness.check(false);
          }
        
        harness.check(false);
      }
    catch (UnsupportedOperationException e)
      {
        harness.check(true);
      }  
  }
}
