/* Copyright (C) 2005 Free Software Foundation, Inc.

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

// Tags: JDK1.4

package gnu.testlet.java.util.LinkedHashMap;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.*;

public class Regress implements Testlet
{
  public void test(TestHarness th)
  {
    // Classpath regression test.
    th.checkPoint("regression test of access order");
    LinkedHashMap hm = new LinkedHashMap(5, (float) 0.5, true);
    hm.put(new Integer(1), new Object());
    hm.put(new Integer(2), new Object());
    hm.put(new Integer(3), new Object());
    hm.get(new Integer(2));
    hm.get(new Integer(3));
    Iterator i = hm.keySet().iterator();
    int count = 1;
    boolean ok = true;
    while (i.hasNext())
      {
	Integer key = (Integer) i.next();
	if (key.intValue() != count)
	  ok = false;
	++count;
      }
    th.check(ok);
  }
}
