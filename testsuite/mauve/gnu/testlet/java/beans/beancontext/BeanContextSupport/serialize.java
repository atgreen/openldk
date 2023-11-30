/* serialize.java -- some checks for the serialize() method in the
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
// Uses: MyBeanContextSupport

package gnu.testlet.java.beans.beancontext.BeanContextSupport;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;
import java.awt.GradientPaint;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;

public class serialize implements Testlet
{
  public void test(TestHarness harness)
  {
    MyBeanContextSupport bcs = new MyBeanContextSupport();
    
    // try null collection
    ByteArrayOutputStream buffer = new ByteArrayOutputStream();
    boolean pass = false;
    try
    {
      ObjectOutputStream out = new ObjectOutputStream(buffer);
      bcs.serializeX(out, null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    catch (IOException e) 
    {
      e.printStackTrace();   
    }
    harness.check(pass);
    
    // try serializing an empty collection
    Collection c1 = new ArrayList();
    buffer = new ByteArrayOutputStream();
    try
    {
      ObjectOutputStream out = new ObjectOutputStream(buffer);
      bcs.serializeX(out, c1);
      out.close();
      ObjectInputStream in = new ObjectInputStream(new ByteArrayInputStream(
              buffer.toByteArray()));
      Collection c2 = new HashSet();
      bcs.deserializeX(in, c2);
      in.close();
      harness.check(c2.isEmpty());
    }
    catch (IOException e) 
    {
      e.printStackTrace();   
    }
    catch (ClassNotFoundException e)
    {
      e.printStackTrace();
    }
    
    // try collection with one item
    c1 = new ArrayList();
    c1.add(new Integer(99));
    buffer = new ByteArrayOutputStream();
    try
    {
      ObjectOutputStream out = new ObjectOutputStream(buffer);
      bcs.serializeX(out, c1);
      out.close();
      ObjectInputStream in = new ObjectInputStream(new ByteArrayInputStream(
              buffer.toByteArray()));
      Collection c2 = new HashSet();
      bcs.deserializeX(in, c2);
      in.close();
      harness.check(c2.contains(new Integer(99)));
    }
    catch (IOException e) 
    {
      e.printStackTrace();   
    }
    catch (ClassNotFoundException e)
    {
      e.printStackTrace();
    }
      
    // try collection with two different items
    c1 = new ArrayList();
    c1.add(new Integer(99));
    c1.add("Item 2");
    buffer = new ByteArrayOutputStream();
    try
    {
      ObjectOutputStream out = new ObjectOutputStream(buffer);
      bcs.serializeX(out, c1);
      out.close();
      ObjectInputStream in = new ObjectInputStream(new ByteArrayInputStream(
              buffer.toByteArray()));
      Collection c2 = new HashSet();
      bcs.deserializeX(in, c2);
      in.close();
      harness.check(c2.contains(new Integer(99)));
      harness.check(c2.contains("Item 2"));
    }
    catch (IOException e) 
    {
      e.printStackTrace();   
    }
    catch (ClassNotFoundException e)
    {
      e.printStackTrace();
    }
      
    // try collection with a non-serializable item
    c1 = new ArrayList();
    c1.add(new GradientPaint(1.0f, 2.0f, Color.red, 3.0f, 4.0f, Color.blue));
    buffer = new ByteArrayOutputStream();
    try
    {
      ObjectOutputStream out = new ObjectOutputStream(buffer);
      bcs.serializeX(out, c1);
      out.close();
      ObjectInputStream in = new ObjectInputStream(new ByteArrayInputStream(
              buffer.toByteArray()));
      Collection c2 = new HashSet();
      bcs.deserializeX(in, c2);
      in.close();
      harness.check(c2.isEmpty());
    }
    catch (IOException e) 
    {
      e.printStackTrace();   
    }
    catch (ClassNotFoundException e)
    {
      e.printStackTrace();
    }
    
    // try null output stream
    pass = false;
    try
    {
      bcs.serializeX(null, new ArrayList());
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    catch (IOException e)
    {
      e.printStackTrace();
    }
    harness.check(pass);
    
    // try reading back a single item into a collection that is not empty
    c1 = new ArrayList();
    c1.add("XYZ");
    buffer = new ByteArrayOutputStream();
    try
    {
      ObjectOutputStream out = new ObjectOutputStream(buffer);
      bcs.serializeX(out, c1);
      out.close();
      ObjectInputStream in = new ObjectInputStream(new ByteArrayInputStream(
              buffer.toByteArray()));
      Collection c2 = new HashSet();
      c2.add("ABC");
      bcs.deserializeX(in, c2);
      in.close();
      harness.check(c2.size(), 2);
      harness.check(c2.contains("XYZ"));
      harness.check(c2.contains("ABC"));
    }
    catch (IOException e) 
    {
      e.printStackTrace();   
    }
    catch (ClassNotFoundException e)
    {
      e.printStackTrace();
    }

    // try serializing a collection containing a null item
    c1 = new ArrayList();
    c1.add("XYZ");
    c1.add(null);
    buffer = new ByteArrayOutputStream();
    try
    {
      ObjectOutputStream out = new ObjectOutputStream(buffer);
      bcs.serializeX(out, c1);
      out.close();
      ObjectInputStream in = new ObjectInputStream(new ByteArrayInputStream(
              buffer.toByteArray()));
      Collection c2 = new HashSet();
      bcs.deserializeX(in, c2);
      in.close();
      harness.check(c2.size(), 1);
      harness.check(c2.contains("XYZ"));
    }
    catch (IOException e) 
    {
      e.printStackTrace();   
    }
    catch (ClassNotFoundException e)
    {
      e.printStackTrace();
    }
      
  }
}
