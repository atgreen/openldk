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

public class copy implements Testlet
{

  public void test(TestHarness harness)
  {
    List l1 = new ArrayList();
    List l2 = new ArrayList();
    
    // copy empty list
    Collections.copy(l2, l1);
    harness.check(l2.isEmpty());
    
    // copy a list with 1 item
    l1.add("A");
    l2.add("B");
    Collections.copy(l2, l1);
    harness.check(l2.get(0).equals("A"));
    
    // check that when destination is longer than source, the extra items are 
    // preserved...
    l1 = new ArrayList();
    l1.add("A");
    l2 = new ArrayList();
    l2.add("B"); l2.add("C"); l2.add("D");
    Collections.copy(l2, l1);
    harness.check(l2.get(0).equals("A"));
    harness.check(l2.get(1).equals("C"));
    harness.check(l2.get(2).equals("D"));
    
    // test where destination is shorter than source
    l1 = new ArrayList();
    l1.add("Item 1");
    l2 = new ArrayList();
    boolean pass = false;
    try
    {
      Collections.copy(l2, l1);
    }
    catch (IndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // test null argument 1
	pass = false;
	try
	{
	  Collections.copy(null, l1);
	}
	catch (NullPointerException e) 
	{
	  pass = true;
	}
	harness.check(pass);
	
	// test null argument 2
	try
	{
	  Collections.copy(l2, null);
	}
	catch (NullPointerException e) 
	{
	  pass = true;
	}
	harness.check(pass);
	
	// try read-only destination
	l1 = new ArrayList(); l1.add("A"); l1.add("B");
	l2 = new ArrayList(); l2.add("C"); l2.add("D");
	l2 = Collections.unmodifiableList(l2);
	pass = false;
	try
	{
	  Collections.copy(l2, l1);
	}
	catch (UnsupportedOperationException e)
	{
	  pass = true;
	}
	harness.check(pass);
	
  }

}
