// Tags: JDK1.5

// Copyright (C) 2010 Andrew John Hughes <gnu_andrew@member.fsf.org>

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

package gnu.testlet.java.util.Collection;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Collection;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Delayed;
import java.util.concurrent.TimeUnit;

/**
 * Some tests for the remove() method in the {@link List} interface.
 */
public class Test implements Testlet
{

  private TestHarness harness;

  /**
   * Runs the test using the specified harness.
   *
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    this.harness = harness;
    testClass(java.util.concurrent.ArrayBlockingQueue.class);
    testClass(java.util.ArrayDeque.class);
    testClass(java.util.ArrayList.class);
    testClass(java.util.concurrent.ConcurrentLinkedQueue.class);
    testClass(java.util.concurrent.ConcurrentSkipListSet.class);
    testClass(java.util.concurrent.CopyOnWriteArrayList.class);
    testClass(java.util.concurrent.CopyOnWriteArraySet.class);
    testClass(java.util.concurrent.DelayQueue.class);
    testClass(java.util.EnumSet.class);
    testClass(java.util.HashSet.class);
    testClass(java.util.concurrent.LinkedBlockingQueue.class);
    testClass(java.util.LinkedHashSet.class);
    testClass(java.util.LinkedList.class);
    testClass(java.util.concurrent.PriorityBlockingQueue.class);
    testClass(java.util.PriorityQueue.class);
    testClass(java.util.Stack.class);
    testClass(java.util.concurrent.SynchronousQueue.class);
    testClass(java.util.TreeSet.class);
    testClass(java.util.Vector.class);
  }

  /**
   * Tests the given {@link java.util.Collection} class.
   *
   * @param cls the class to test.
   */
  public void testClass(Class<? extends Collection> cls)
  {
    harness.checkPoint(cls.getName());
    Collection result = null;
    try
    {
      result = (Collection) cls.newInstance();
    }
    catch (Exception e)
    {
      harness.debug(e);
    }
    if (result != null)
      {
        testRemove(result);
      }
  }

  /**
   * Test the {@link Collection#remove(Object)} method of
   * the given collection.
   *
   * @param coll the collection to test.
   */
  public void testRemove(Collection coll)
  {
    /**
     * Use Delayed Object so DelayQueue
     * and sorted collections work.
     */
    Delayed obj = new Delayed()
      {
        public long getDelay(TimeUnit unit)
        {
          return unit.convert(10, TimeUnit.MINUTES);
        }
        public int compareTo(Delayed o)
        {
          Long other = o.getDelay(TimeUnit.NANOSECONDS);
          return other.compareTo(getDelay(TimeUnit.NANOSECONDS));
        }
      };
    String type = coll.getClass().getName();

    harness.check(coll.remove(obj) == false,
                  "Object is not present in empty " + type);

    boolean result = false;
    try
      {
        result = coll.remove(null);
      }
    catch (NullPointerException e)
      {
        /* Collection does not support null elements */
      }
    harness.check(result == false, "Null is not present in empty " + type);

    /* Can't do post-addition tests if no capacity */
    if (coll instanceof BlockingQueue &&
        ((BlockingQueue) coll).remainingCapacity() == 0)
      return;

    coll.add(obj);
    harness.check(coll.remove(obj) == true,
                  "Object is present in non-empty " + type);

    result = true;
    try
      {
        coll.add(null);
        result = coll.remove(null);
      }
    catch (NullPointerException e)
      {
        /* Collection does not support null elements */
      }
    harness.check(result == true, "Null is present in non-empty " + type);
  }

}
