/* Copyright (c) 2006 Mark J. Wielaard  (mark@klomp.org)

   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

// Tags: JDK1.2

package gnu.testlet.java.util.AbstractCollection;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.*;

/**
 * Checks that toString() can handle collections that contain themselves.
 */
public class toString implements Testlet, Comparator
{
  public void test (TestHarness harness)
  {
    testCollection(new LinkedList(), harness);
    testCollection(new ArrayList(), harness);
    testCollection(new Vector(), harness);
    testCollection(new Stack(), harness);
    testCollection(new HashSet(), harness);
    testCollection(new LinkedHashSet(), harness);
    testCollection(new TreeSet(this), harness);
  }

  private void testCollection(Collection c, TestHarness h)
  {
    h.checkPoint(c.getClass().getName());
    c.add(new Integer(123));
    c.add(c);
    c.add("abc");
    String s = c.toString();
    h.debug(s);
    h.check(s.indexOf("123") != -1);
    h.check(s.indexOf("abc") != -1);
  }

  public int compare(Object o1, Object o2)
  {
    return String.valueOf(o1).compareTo(String.valueOf(o2));
  }

  public boolean equals(Object o)
  {
    return o == this;
  }
}
