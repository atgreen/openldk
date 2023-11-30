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
import java.util.NoSuchElementException;

public class min implements Testlet
{

  public void test(TestHarness harness) 
  {
    List list = new ArrayList();
    
    // try an empty list
    boolean pass = false;
    try
    {
      Object m = Collections.min(list);
    }
    catch (NoSuchElementException e)
    {
      pass = true;
    }
    harness.check(true);
    
    // try a regular list
    list.add(new Integer(12));
    list.add(new Integer(9));
    list.add(new Integer(17));
    harness.check(Collections.min(list).equals(new Integer(9)));
 
    // try a null list
    pass = false;
    try
    {
      Object ignore = Collections.min(null);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // try a list with non-comparable items
    list.clear();
    list.add("A"); list.add(new Long(1));
    pass = false;
    try
    {
      Object ignore = Collections.min(list);
    }
    catch (ClassCastException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
  }
}
