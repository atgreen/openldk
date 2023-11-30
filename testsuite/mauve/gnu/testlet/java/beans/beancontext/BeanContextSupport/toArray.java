/* toArray.java -- some checks for the toArray() methods in the 
       BeanContextSupport class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
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

// Tags: JDK1.2

package gnu.testlet.java.beans.beancontext.BeanContextSupport;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.beancontext.BeanContextSupport;
import java.util.Arrays;
import java.util.List;

public class toArray implements Testlet
{
  public void test(TestHarness harness)
  {
    testMethod1(harness);
    testMethod2(harness);
  }
  
  public void testMethod1(TestHarness harness)
  {
    harness.checkPoint("()");
    BeanContextSupport bcs = new BeanContextSupport();
    Object[] array = bcs.toArray();
    harness.check(array.length, 0);
    BeanContextSupport child1 = new BeanContextSupport();
    bcs.add(child1);
    array = bcs.toArray();
    harness.check(array.length, 1);
    harness.check(array[0] == child1);
    bcs.add("Child2");
    array = bcs.toArray();
    harness.check(array.length, 2);
    // use a list to check membership, because the order is not guaranteed
    List children = Arrays.asList(array);
    harness.check(children.contains(child1));    
    harness.check(children.contains("Child2"));    
  }
  
  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(Object[])");
    BeanContextSupport bcs = new BeanContextSupport();
    
    // try null
    boolean pass = false;
    try
    {
      bcs.toArray(null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try zero length array for zero children
    Object[] array1 = new Object[0];
    Object[] array2 = bcs.toArray(array1);
    harness.check(array2 == array1);
    
    // try array length 1 for zero children
    array1 = new Object[1];
    array2 = bcs.toArray(array1);
    harness.check(array2 == array1);
    harness.check(array2[0], null);
    
    // try array length 0 for 1 child
    BeanContextSupport child1 = new BeanContextSupport();
    bcs.add(child1);
    array1 = new Object[0];
    array2 = bcs.toArray(array1);
    harness.check(array2 != array1);
    harness.check(array2.length, 1);
    harness.check(array2[0], child1);
    
    // try array length 1 for 1 child
    array1 = new Object[1];
    array2 = bcs.toArray(array1);
    harness.check(array2 == array1);
    harness.check(array2[0], child1);
    
    // try array length 2 for 1 child
    array1 = new Object[2];
    array2 = bcs.toArray(array1);
    harness.check(array2 == array1);
    harness.check(array2[0], child1);
    harness.check(array2[1], null);
    
    // try array length 2 for 2 children
    bcs.add("Child 2");
    array1 = new Object[2];
    array2 = bcs.toArray(array1);
    harness.check(array2 == array1);
    // use a list to check membership, because the order is not guaranteed
    List children = Arrays.asList(array2);
    harness.check(children.contains(child1));    
    harness.check(children.contains("Child 2"));    
    
    // try an array of the wrong type
    String[] array3 = new String[2];
    pass = false;
    try
    {
      bcs.toArray(array3);
    }
    catch (ArrayStoreException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
