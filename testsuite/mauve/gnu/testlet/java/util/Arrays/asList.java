//Tags: JDK1.4

//Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.

package gnu.testlet.java.util.Arrays;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import java.util.RandomAccess;

/**
 * Some tests for the asList() method in the {@link Arrays} class.
 */
public class asList implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    Object[] a1 = new Object[] {"1", "2", "3"}; 
    List l1 = Arrays.asList(a1);
    
    // check that the list is the same as the array...
    harness.check(l1.size() == 3);
    harness.check(l1.get(0).equals("1"));
    harness.check(l1.get(1).equals("2"));
    harness.check(l1.get(2).equals("3"));
    harness.check(l1 instanceof RandomAccess);
    harness.check(l1 instanceof Serializable);
    
    // a change to the list updates the array...
    l1.set(1, "99");
    harness.check(a1[1].equals("99"));
    
    // a change to the array updates the list...
    a1[1] = "100";
    harness.check(l1.get(1).equals("100"));
    
    // check unsupported operations
    boolean pass = false;
    try
    {
      l1.add("new item");
    }
    catch (UnsupportedOperationException e)  
    {
      pass = true;
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      l1.clear();
    }
    catch (UnsupportedOperationException e)  
    {
      pass = true;
    }
    harness.check(pass);

    try
    {
      l1.remove(0);
    }
    catch (UnsupportedOperationException e)  
    {
      pass = true;
    }
    harness.check(pass);

    try
    {
      l1.remove("1");
    }
    catch (UnsupportedOperationException e)  
    {
      pass = true;
    }
    harness.check(pass);
    
    // check null argument
    pass = false;
    try
    {
      Arrays.asList(null);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);  
  }

}