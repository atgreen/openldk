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
import java.util.List;

public class reverse implements Testlet
{

  public void test(TestHarness harness) 
  {
    List list1 = new ArrayList();
    list1.add("t"); list1.add("a"); list1.add("n"); list1.add("k"); list1.add("s");
    List list2 = new ArrayList();
    list2.add("s"); list2.add("k"); list2.add("n"); list2.add("a"); list2.add("t"); 
    Collections.reverse(list1);
    harness.check(list1.equals(list2));    // check 1
 
    // try an empty list
    list1 = new ArrayList();
    Collections.reverse(list1);
    harness.check(list1.isEmpty());        // check 2
 
    // try a null list
    boolean pass = false;
    try
    {
      Collections.reverse(null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);                   // check 3
 
    // try an unmodifiable list
    list1 = Collections.unmodifiableList(list1);
    pass = false;
    try
    {
      Collections.reverse(null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);                   // check 4

  }
}
