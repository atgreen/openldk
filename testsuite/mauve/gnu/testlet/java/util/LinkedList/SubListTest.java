// Tags: JDK1.2

// Copyright (C) 2003 Daniel Bonniot <bonniot@users.sf.net>

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

package gnu.testlet.java.util.LinkedList;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.*;

public class SubListTest implements Testlet
{
  public void test (TestHarness harness)
  {
    test(harness, new LinkedList());
  }

  /* This method could be used to test subList on any implementation of List.*/
  public static void test (TestHarness harness, List list)
  {
    list.clear();
    list.add("0");
    list.add("1");
    list.add("2");
    list.add("3");
    
    final int start = 1, end = 3;

    List sub = list.subList(start,end);
    harness.check(sub.get(0).equals(list.get(start)));

    Iterator it = sub.iterator();
    int i = start;
    while (it.hasNext())
      {
        harness.check(it.next().equals(list.get(i)));
        i++;
      }

    harness.check(i == end);
  }
}
