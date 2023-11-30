/* Equals.java -- test for equals.
   Copyright (C) 2008 Mario Torre <mario.torre@aicas.com>
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
import java.util.concurrent.CopyOnWriteArrayList;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * @author Mario Torre <neugens@aicas.com>
 */
public class Equals
    implements Testlet
{
  public void test(TestHarness harness)
  {
    CopyOnWriteArrayList<String> one = new CopyOnWriteArrayList<String>();
    for (int i = 0; i < 10; i++)
      {
        one.add("#" + i);
      }
    
    CopyOnWriteArrayList<String> two =
      (CopyOnWriteArrayList<String>) one.clone();
    
    harness.checkPoint("cloned CopyOnWriteArrayList");
    harness.check(one.equals(two));
    
    two.clear();
    two = new CopyOnWriteArrayList<String>();
    for (int i = 0; i < 10; i++)
      {
        two.add("#" + i);
      }
    
    harness.checkPoint("both CopyOnWriteArrayList, same elements, different " +
    		           "instances");
    harness.check(one.equals(two));
    
    List<String> someList = new ArrayList<String>();
    for (int i = 0; i < 10; i++)
      {
        someList.add("#" + i);
      }
    
    harness.checkPoint("Different list, CopyOnWriteArrayList and ArrayList, " +
                       "but same elements");
    harness.check(one.equals(someList));
    
    // remove one element
    one.remove(one.size() - 1);
    
    harness.checkPoint("removed elements from CopyOnWriteArrayList");
    harness.check(!one.equals(two));
    harness.check(!one.equals(someList));
  }
}
