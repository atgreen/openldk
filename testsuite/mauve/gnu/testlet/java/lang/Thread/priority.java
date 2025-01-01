// Tags: JDK1.0

// Copyright (C) 2002 Free Software Foundation, Inc.
// Written by Mark Wielaard (mark@klomp.org)

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

package gnu.testlet.java.lang.Thread;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class priority implements Testlet, Runnable
{
  private static TestHarness harness;

  private void test_set_prio(Thread t, String test_name)
  {
    int prio = t.getPriority();
    harness.check(prio >= Thread.MIN_PRIORITY,
		    test_name + " has at least MIN_PRIORITY");
    harness.check(prio <= Thread.MAX_PRIORITY,
		    test_name + " has at at most MAX_PRIORITY");

    // Remember old thread group priority
    ThreadGroup tg = t.getThreadGroup();
    harness.debug("ThreadGroup: " + tg);
    int thread_group_prio = t.getThreadGroup().getMaxPriority();

    t.getThreadGroup().setMaxPriority(Thread.MAX_PRIORITY);
    for (int i = Thread.MIN_PRIORITY; i <= Thread.MAX_PRIORITY; i++)
      {
	t.setPriority(i);
	harness.check(t.getPriority() == i,
			test_name + " can be set to priority " + i);
      }

    t.getThreadGroup().setMaxPriority(Thread.NORM_PRIORITY);
    t.setPriority(Thread.NORM_PRIORITY+1);
    harness.check(t.getPriority() == Thread.NORM_PRIORITY,
		    test_name + " has thread group upper bound");

    // Reset thread group priority to not disturb other tests.
    t.getThreadGroup().setMaxPriority(thread_group_prio);

    boolean illegal_exception = false;
    try
      {
	t.setPriority(Thread.MIN_PRIORITY-1);
      }
    catch (IllegalArgumentException iae)
      {
	illegal_exception = true;
      }
    harness.check(illegal_exception,
		    test_name + " cannot set prio to less then MIN_PRIORITY");
    harness.check(t.getPriority() == Thread.NORM_PRIORITY,
		    test_name + " prio doesn't change when set to illegal min");

    illegal_exception = false;
    try
      {
	t.setPriority(Thread.MAX_PRIORITY+1);
      }
    catch (IllegalArgumentException iae)
      {
	illegal_exception = true;
      }
    harness.check(illegal_exception,
		    test_name + " cannot set prio to more then MAX_PRIORITY");
    harness.check(t.getPriority() == Thread.NORM_PRIORITY,
		    test_name + " prio doesn't change when set to illegal max");
  }

  public void test (TestHarness h)
  {
    harness = h;

    harness.check(10, Thread.MAX_PRIORITY);
    harness.check(1, Thread.MIN_PRIORITY);
    harness.check(5, Thread.NORM_PRIORITY);

    Thread current = Thread.currentThread();
    test_set_prio(current, "Every Thread");

    int prio = current.getPriority();
    Thread t  = new Thread(p);
    harness.check(t.getPriority() == prio,
		    "New Thread inherits priority");
    test_set_prio(t, "New Thread");

    prio = t.getPriority();
    t.start();
    harness.check(t.getPriority() == prio,
		    "Started Thread does not change priority");
    test_set_prio(t, "Started Thread");

    synchronized(p) {
      p.please_stop = true;
      p.notify();
    }

    try { t.join(); } catch(InterruptedException ie) { }
    harness.check(t.getPriority() == prio,
                    "Stopped Thread does not change priority");

    // What is the expected behavior of setPriority on a stopped Thread?
  }

  static priority p = new priority();
  boolean please_stop = false;
  public void run()
  {
    synchronized(p)
    {
      while(!please_stop)
	try { p.wait(); } catch(InterruptedException ie) { }
    }
  }
}

