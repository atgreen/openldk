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

public class join extends Thread implements Testlet
{
  static TestHarness harness;

  // Set this to true if want this thread to notify and wait till please_stop
  boolean please_wait = false;
  boolean waiting = false;
  boolean please_stop = false;

  // Set to a value bigger then zero if you want the thread to sleep or work
  // a bit before terminating.
  int sleep = 0;
  int work = 0;

  // When set will join this Thread.
  Thread please_join = null;

  // Set when this Thread thinks it is done.
  boolean done = false;

  // Counter that is used to do some 'work'
  long counter = 0;

  public void run()
  {
    if (please_wait)
      synchronized(this)
	{
	  waiting = true;
	  this.notify();

	  while(!please_stop)
	    try
	      {
		this.wait();
	      }
	    catch (InterruptedException ie)
	      {
		harness.fail("Interrupted wait in run()");
	      }
	}

    if (sleep > 0)
      try
	{
	  Thread.sleep(sleep);
	}
      catch (InterruptedException ignore)
        {
	  harness.fail("Interrupted sleep in run()");
	}

    if (work > 0)
      for (int i = 0; i < work; i++)
	counter += (sleep < 0 ? (work - 1) : work +1); // silly computation.

    if (please_join != null)
      try
	{
	  please_join.join();
	}
      catch (InterruptedException ignore)
        {
	  harness.fail("Interrupted join in run()");
	}

    done = true;
  }

  public void test (TestHarness h)
  {
    harness = h;

    try
      {
	Thread current = Thread.currentThread();
	current.join(10,10);
	harness.check(current.isAlive(), "Can join current Thread");

	join t = new join();
	t.join(10,10);
	harness.check(!t.isAlive(), "Can join a non-started Thread");

	t.start();
	t.join();
	harness.check(!t.isAlive(), "Can join a started Thread");
	harness.check(t.done, "join() returns after Thread is done");

	t.join();
	harness.check(!t.isAlive(), "Can join a stopped Thread");

	t = new join();
	t.please_wait = true;
	t.start();
	synchronized(t)
	  {
	    while (!t.waiting)
	      t.wait();
	  }
	t.join(10,10);
	harness.check(t.waiting && !t.done, "Can join waiting Thread");
	synchronized(t)
	  {
	      t.please_stop = true;
	      t.notify();
	  }
	t.join();
	harness.check(!t.isAlive(), "Can join wait/notify Thread");
	harness.check(t.done, "join() returns after wait/notify Thread done");

	t = new join();
	t.work = 100000;
	t.start();
	t.join();
	harness.check(t.done, "join() returns after Thread has worked");

	t = new join();
	t.sleep = 750;
	t.start();
	t.join();
	harness.check(t.done, "join() returns after Thread has slept");

	join t1 = new join();
	t1.sleep = 750;
	t1.work = 100000;
	join t2 = new join();
	t2.please_join = t1;
	t1.start();
	t2.start();
	t1.join();
	t2.join();
	harness.check(t1.done && t2.done, "Multiple Threads can join()");

      }
    catch (InterruptedException ie)
      {
	harness.fail("Unexpected interrupt");
      }
  }
}

