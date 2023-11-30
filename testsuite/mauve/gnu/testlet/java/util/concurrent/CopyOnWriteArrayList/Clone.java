/* Clone.java -- test for clone.
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

import java.util.Iterator;
import java.util.concurrent.CopyOnWriteArrayList;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * @author Mario Torre <neugens@aicas.com>
 */
public class Clone
    implements Testlet
{
  public void test(TestHarness harness)
  {
    CopyOnWriteArrayList<String> list = new CopyOnWriteArrayList<String>();
    for (int i = 0; i < 10; i++)
      {
        list.add("#" + i);
      }
    
    CopyOnWriteArrayList<String> cloned =
      (CopyOnWriteArrayList<String>) list.clone();
    
    harness.check(list.size() == 10);
    harness.check(cloned.size() == list.size());
    
    Iterator<String> original = list.iterator();
    for (String element : cloned)
      {
        harness.check(original.hasNext());
        harness.check(element, original.next());
      }
  }
}
