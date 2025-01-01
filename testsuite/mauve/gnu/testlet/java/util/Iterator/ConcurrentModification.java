// Tags: JDK1.2

// Copyright (C) 2006 Red Hat, Inc.

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
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.util.Iterator;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.*;

/**
 *  For a variety of collections classes, this test modifies the backing 
 *  store underlying an active iterator, and check that Iterator.next()
 *  correctly throws ConcurrentModificationException, while hasNext(), 
 *  hasPrevious(), previousIndex(), and nextIndex() do not.
 */
public class ConcurrentModification implements Testlet
{
  TestHarness harness;
  
  public void test(TestHarness harness)      
  {
    this.harness = harness;
    testMapIterator(new HashMap());
    testMapIterator(new TreeMap());
    testMapIterator(new Hashtable());
    testMapIterator(new LinkedHashMap());
    testMapIterator(new IdentityHashMap());
    testMapIterator(new WeakHashMap());
    testMapIterator(Collections.synchronizedMap(new HashMap()));
    testListIterator(new ArrayList());
    testListIterator(new Vector());
    testListIterator(new LinkedList());
    testListIterator(Collections.synchronizedList(new ArrayList()));
    testCollectionIterator(new HashSet());
    testCollectionIterator(new LinkedHashSet());
    testCollectionIterator(new TreeSet());
  }
  
  void testMapIterator(Map map)
  {
    map.put("1", "value");
    map.put("2", "value");
    testIterator(map.keySet());
    map.clear();
    map.put("1", "value");
    map.put("2", "value");
    testIterator(map.values());
  }
  
  void testListIterator(List l)
  {
    l.add("1");
    l.add("2");
    testIterator(l);
    l.clear();
    l.add("1");
    l.add("2");
    l.add("3");
    testIterator(l.subList(0, 3));
    l.clear();
    l.add("1");
    l.add("2");
    l.add("3");
    testListHasPrevious(l);
  }
  
  void testCollectionIterator(Collection c)
  {
    c.add("1");
    c.add("2");
    testIterator(c);
  }
  
  void testIterator(Collection c)
  {
    Iterator iter = c.iterator();

    String element = (String) iter.next();
    c.remove(element); // Invalid concurrent modification.

    boolean hasNext = false;
    try
    {
      hasNext = iter.hasNext();
    }
    catch (ConcurrentModificationException x)
    {
      harness.fail(c.getClass() + ".iterator().hasNext() throws " + x);
      return;
    }
    
    try
    {
      element = (String) iter.next();
    }
    catch (ConcurrentModificationException x)
    {
      harness.check(true); // OK!
    }
  }

  void testListHasPrevious(List l)
  {
    ListIterator iter = l.listIterator();

    String element = (String) iter.next();
    l.remove(element); // Invalid concurrent modification.

    int idx = -1;
    boolean hasPrevious = false;
    try
    {
      hasPrevious = iter.hasPrevious();
    }
    catch (ConcurrentModificationException x)
    {
      harness.fail(l.getClass() + ".listIterator().hasPrevious() throws " + x);
      return;
    }

    try
    {
      idx = iter.nextIndex();
    }
    catch (ConcurrentModificationException x)
    {
      harness.fail(l.getClass() + ".listIterator().nextIndex() throws " + x);
      return;
    }

    try
    {
      idx = iter.previousIndex();
    }
    catch (ConcurrentModificationException x)
    {
      harness.fail(l.getClass() + ".listIterator().previousIndex() throws " + x);
      return;
    }
    
    try
    {
      element = (String) iter.next();
    }
    catch (ConcurrentModificationException x)
    {
      harness.check(true); // OK!
    }    
  }
}
