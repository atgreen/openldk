/* unmodifiableMap.java -- some checks for the unmodifiableMap() method in the
                           Collections class.
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

package gnu.testlet.java.util.Collections;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class unmodifiableMap implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("Empty map");
    HashMap map = new HashMap();
    testMap(map, harness);
    
    harness.checkPoint("Non-empty map");
    map.put("A", "AA");
    map.put("B", "BB");
    map.put("C", "CC");
    testMap(map, harness);
       
    harness.checkPoint("Null map");
    boolean pass = false;
    try
      {
        Collections.unmodifiableMap(null);
      }
    catch (NullPointerException e)
      {
        pass = true;
      }
    harness.check(pass);
  
  }
  
  private void testMap(Map map, TestHarness harness)
  {
    Map umap = Collections.unmodifiableMap(map);
    
    // check clear() method
    boolean pass = false;
    try
      {
        umap.clear();
      }
    catch (UnsupportedOperationException e)
      {
        pass = true;
      }
    harness.check(pass);
    
    // check put() method
    pass = false;
    try
      {
        umap.put("X", "Y");
      }
    catch (UnsupportedOperationException e)
      {
        pass = true;
      }
    harness.check(pass);
    
    // check putAll() method
    HashMap map2 = new HashMap();
    map2.put("ONE", new Integer(1));
    pass = false;
    try
      {
        umap.putAll(map2);
      }
    catch (UnsupportedOperationException e)
      {
        pass = true;    
      }
    harness.check(pass);
    
    // check the Map.Entry items from entrySet()
    pass = false;
    Iterator iterator = umap.entrySet().iterator();
    if (iterator.hasNext())
      {
        Map.Entry entry = (Map.Entry) iterator.next();
        try
          {
            entry.setValue("XYZ");
          }
        catch (UnsupportedOperationException e)
          {
            pass = true;  
          }
        harness.check(pass);
      }
    
    // check a Map.Entry item from entrySet().toArray()
    pass = false;
    Object[] entries = umap.entrySet().toArray();
    if (entries.length > 0)
      {
        try
          {
            ((Map.Entry) entries[0]).setValue("XYZ");
          }
        catch (UnsupportedOperationException e)
          {
            pass = true;
          }
        harness.check(pass);
      }

    // check a Map.Entry item from entrySet().toArray(Object[])
    pass = false;    
    Object[] entries2 = new Object[umap.size()];
    umap.entrySet().toArray(entries2);
    if (entries2.length > 0)
      {
        try
          {
            ((Map.Entry) entries2[0]).setValue("XYZ");
          }
        catch (UnsupportedOperationException e)
          {
            pass = true;
          }
        harness.check(pass);
      }
        
  }
}
