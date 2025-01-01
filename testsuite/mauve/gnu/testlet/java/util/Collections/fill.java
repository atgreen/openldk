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

public class fill implements Testlet
{

  public void test(TestHarness harness)
  {
    List list = new ArrayList();
 
    // fill empty list
    Collections.fill(list, "X");
    harness.check(list.isEmpty());
 
    // fill a list with 1 item
    list.add("A");
    Collections.fill(list, "X");
    harness.check(list.get(0).equals("X"));
 
    // fill a list with multiple items
    list = new ArrayList();
    list.add("A"); list.add("B"); list.add("C");

    Collections.fill(list, "X");
    harness.check(list.get(0).equals("X"));
    harness.check(list.get(1).equals("X"));
    harness.check(list.get(2).equals("X"));
  
    // test null argument 1
	boolean pass = false;
	try
	{
	  Collections.fill(null, "X");
	}
	catch (NullPointerException e) 
	{
	  pass = true;
	}
	harness.check(pass);
	
	// test null argument 2
	Collections.fill(list, null);
    harness.check(list.get(0) == null);
    harness.check(list.get(1) == null);
    harness.check(list.get(2) == null);
		
	// try read-only destination
	list = new ArrayList(); list.add("A"); list.add("B");
	list = Collections.unmodifiableList(list);
	pass = false;
	try
	{
	  Collections.fill(list, "X");
	}
	catch (UnsupportedOperationException e)
	{
	  pass = true;
	}
	harness.check(pass);
	
}

}
