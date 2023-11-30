/* RemoveTest.java -- test for remove.
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
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class RemoveTest implements Testlet
{
  public void test(TestHarness harness)
  {
    CopyOnWriteArrayList<Integer> list =
      new CopyOnWriteArrayList<Integer>();

    List<Integer> data = new ArrayList<Integer>();
    for (int i = 0; i < 10; i++)
      data.add(i);

    list.addAll(data);
    
    harness.check(list.size() == 10);
    
    Integer el = list.remove(5);
    
    harness.check(el.intValue() == 5);
    harness.check(list.size() == 9);
    
    harness.check(list.add(el));
    harness.check(list.size() == 10);
    
    harness.check(list.remove(el));
    harness.check(list.size() == 9);
    
    int [] expected =
    { 
     0, 1, 2, 3, 4, 6, 7, 8, 9
    };
    
    int i = 0;
    for (Iterator<Integer> iterator = list.iterator(); iterator.hasNext(); )
      harness.check(iterator.next().intValue() == expected[i++]);
  }
}
