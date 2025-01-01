// Tags: JDK1.0

// Copyright (C) 2003 Free Software Foundation, Inc.
// Written by C. Brian Jones (cbj@gnu.org)

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

public class stop extends Thread implements Testlet
{
  static TestHarness harness;
  // accessed outside synchronized block by multiple threads
  static volatile boolean death = false;

  static Object lock = new Object();
  static boolean running = false;

  public void run()
  {
    try
      {
	synchronized(lock)
	  {
	    running = true;
	    lock.notifyAll();
	    while (true)
	      lock.wait();
	  }
      }
    catch (Exception e)
      {
	harness.fail("Unexpected exception during run()");
      }
    catch (ThreadDeath d)
      {
	death = true;
	Thread thread = Thread.currentThread();
	ThreadGroup group = thread.getThreadGroup();
	harness.check(group != null, "Stop should not remove thread from ThreadGroup");
	throw d;
      }
  }

  public void test (TestHarness h)
  {
    harness = h;
    int initial_thread_count = 0;
    int running_thread_count = 0;
    int stopped_thread_count = 0;
    Thread[] thread_list = null;

    try
      {
	int x = 0;
	Thread current = Thread.currentThread();
	ThreadGroup group = current.getThreadGroup();
	x = group.activeCount() + 10;
	thread_list = new Thread[x];
	initial_thread_count = group.enumerate(thread_list, true);

	stop t = new stop();
	ThreadGroup tgroup = t.getThreadGroup();
	harness.check (tgroup != null, "Unstarted thread has non-null thread group");
	t.start();
	synchronized(lock)
	  {
	    while (!running) 
	      {
		lock.wait();
	      }
	    x = group.activeCount() + 10;
	    thread_list = new Thread[x];
	    running_thread_count = group.enumerate(thread_list, true);
	    tgroup = t.getThreadGroup();
	    harness.check(tgroup != null, "Running thread has non-null thread group");
	  }
	t.stop();
	t.join(2000, 0);
	x = group.activeCount() + 10;
	thread_list = new Thread[x];
	stopped_thread_count = group.enumerate(thread_list, true);

	harness.check(death, "ThreadDeath properly thrown and caught");
	harness.check(initial_thread_count == stopped_thread_count, 
		      "Initial thread count matches stopped thread count");
	harness.check(running_thread_count-1 == initial_thread_count,
		      "Running thread properly increased thread count");

	tgroup = t.getThreadGroup();
	harness.check(tgroup == null, "Stopped thread has null thread group");

      }
    catch (InterruptedException e) 
      {
	harness.fail("Thread not joined - Thread.stop() unimplemented?");
      }
    catch (Exception e)
      {
        harness.debug(e);
	harness.fail("Unexpected exception during test()");
      }
  }
}

