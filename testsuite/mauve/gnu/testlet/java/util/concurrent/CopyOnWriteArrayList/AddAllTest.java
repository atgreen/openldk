/* AddAllTest.java -- test for addAll.
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
import java.util.List;
import java.util.ListIterator;
import java.util.concurrent.CopyOnWriteArrayList;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class AddAllTest implements Testlet
{
  public void test(TestHarness harness)
  {
    testAdd(harness);
    testExceptions(harness);
  }

  private void testExceptions(TestHarness harness)
  {
    CopyOnWriteArrayList<Integer> list =
      new CopyOnWriteArrayList<Integer>();
    
    List<Integer> list2 = new ArrayList<Integer>();
    list2.add(0);

    harness.checkPoint("addAll - IndexOutOfBoundsException");
    
    try
      {
        // try with index < 0 first
        list.addAll(-1, list2);
        
        // we should not get here
        harness.check(false);
      }
    catch (IndexOutOfBoundsException e)
      {
        harness.check(true);
      }
    catch (Exception e)
      {
        harness.check(false, "Exception of unexpected type: " + e.getMessage());
      }
     
    list.add(0);
    list.add(1);
    
    try
      {
        // try with index > list.size() first
        list.addAll(list.size() + 1, list2);
      
        // we should not get here
        harness.check(false);
      }
    catch (IndexOutOfBoundsException e)
      {
        harness.check(true);
      }
    catch (Exception e)
      {
        harness.check(false, "Exception of unexpected type: " + e.getMessage());
      }
    
    harness.checkPoint("addAll - NullPointerException");
    
    try
      {
        // finally try NullPointerException
        list.addAll(null);
      
        // we should not get here
        harness.check(false);
      }
    catch (NullPointerException e)
      {
        harness.check(true);
      }
    catch (Exception e)
      {
        harness.check(false, "Exception of unexpected type: " + e.getMessage());
      }
  }

  private void testAdd(TestHarness harness)
  {
    harness.checkPoint("addAll");
    
    int [] expected =
    { 
     0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14
    };
    
    CopyOnWriteArrayList<Integer> list =
      new CopyOnWriteArrayList<Integer>();

    for (int i = 0; i < 10; i++)
      list.add(i);
    
    List<Integer> list2 = new ArrayList<Integer>();
    for (int i = 5; i < 15; i++)
      list2.add(i);

    list.addAll(list2);
    
    harness.check(list.size() == 20);
    
    int i = 0;
    for (ListIterator<Integer> elements = list.listIterator();
         elements.hasNext();)
      {
        harness.check(elements.next().intValue() == expected[i++]);
      }
  }
}
