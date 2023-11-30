// Tags: JDK1.0

// Copyright (C) 2004 Free Software Foundation, Inc.
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
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.lang.Thread;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class sleep implements Testlet, Runnable
{
  private TestHarness harness;

  private Thread thread;
  private boolean helper_started;
  private boolean helper_done;

  private final static long SLEEP_TIME = 5 * 1000; // 5 seconds

  // Time the helper thread should sleep before interrupting the main
  // thread Or zero for immediate interruption (won't use
  // synchronization either in that case).
  private long helper_sleep = 0;

  // Helper method that runs from another thread.
  // Sleeps a bit and then interrupts the main thread.
  public void run()
  {
    try
      {
	if (helper_sleep == 0)
	  {
	    thread.interrupt();
	    helper_done = true;
	    return;
	  }
	
	// Make sure main thread know we are about to sleep.
	// (It should also go to sleep)
	synchronized(this)
	  {
	    helper_started = true;
	    this.notify();
	  }

	Thread.sleep(helper_sleep);
	thread.interrupt();

	// Main thread should still have the lock on this
	synchronized(this)
	  {
	    helper_done = true;
	  }
      }
    catch (InterruptedException ie)
      {
	harness.debug("Interrupted in helper thread");
	harness.check(false);
      }
  }

  public void test (TestHarness h)
  {
    harness = h;
    Thread helper = new Thread(this);

    harness.checkPoint("Interrupted sleep");

    // Get a lock on this to coordinate with the runner thread.
    // We should not loose it while sleeping.
    synchronized(this)
      {
	helper_done = false;
	helper_sleep = SLEEP_TIME / 2;
	thread = Thread.currentThread();
	long past = System.currentTimeMillis();
	
	helper.start();
	
	// Wait for the helper to start (and sleep immediately).
	try
	  {
	    while (!helper_started)
	      this.wait();
	  }
	catch (InterruptedException ie)
	  {
	    harness.debug("Interrupted during helper start");
	    harness.check(false);
	  }
	
	// Go to sleep.
	// Helper thread sleeps less time and should interrupt us.
	boolean interrupted_exception = false;
	try
	  {
	    Thread.sleep(SLEEP_TIME);
	  }
	catch (InterruptedException ie)
	  {
	    interrupted_exception = true;
	  }
	harness.check(interrupted_exception);
	
	// About half the time should have been spent sleeping.
	long present = System.currentTimeMillis();
	long diff = present - past;
	harness.debug("diff: " + diff);
	harness.check(diff >= SLEEP_TIME / 2);
	harness.check(diff < SLEEP_TIME);

	// Even though we are interrupted,
	// the thread interrupted flag should be cleared.
	harness.check(!Thread.interrupted());
	
	// We are still holding the lock so the helper_thread
	// cannot be done yet.
	harness.check(!helper_done);
      }
    
    // Now wait for the helper thead to finish
    try
      {
	helper.join();
      }
    catch(InterruptedException ie)
      {
	harness.debug("Interruped during joining the helper thread");
	harness.check(false);
      }
    harness.check(helper_done);
    
    // Invalid argument checks.
    harness.checkPoint("Invalid argument");
    invalid(Long.MIN_VALUE);
    invalid(-1);

    invalid(Long.MIN_VALUE, Integer.MIN_VALUE);
    invalid(Long.MIN_VALUE, -1);
    invalid(Long.MIN_VALUE, 0);
    invalid(Long.MIN_VALUE, 1);
    invalid(Long.MIN_VALUE, 999999);
    invalid(Long.MIN_VALUE, 1000000);
    invalid(Long.MIN_VALUE, Integer.MAX_VALUE);

    invalid(-1, Integer.MIN_VALUE);
    invalid(-1, -1);
    invalid(-1, 0);
    invalid(-1, 1);
    invalid(-1, 999999);
    invalid(-1, 1000000);
    invalid(-1, Integer.MAX_VALUE);

    invalid(0, Integer.MIN_VALUE);
    invalid(0, -1);
    invalid(0, 1000000);
    invalid(0, Integer.MAX_VALUE);

    invalid(1, Integer.MIN_VALUE);
    invalid(1, -1);
    invalid(1, 1000000);
    invalid(1, Integer.MAX_VALUE);

    invalid(Long.MAX_VALUE, Integer.MIN_VALUE);
    invalid(Long.MAX_VALUE, -1);
    invalid(Long.MAX_VALUE, 1000000);
    invalid(Long.MAX_VALUE, Integer.MAX_VALUE);

    // (Large) valid argument checks
    valid(Integer.MAX_VALUE);
    valid(Long.MAX_VALUE);
    valid(Integer.MAX_VALUE, 0);
    valid(Long.MAX_VALUE, 0);
    valid(Integer.MAX_VALUE, 1);
    valid(Long.MAX_VALUE, 1);
    valid(Integer.MAX_VALUE, 999999);
    valid(Long.MAX_VALUE, 999999);

    // (Near) zero argument checks.
    harness.checkPoint("(Near) zero sleep");
    long past = System.currentTimeMillis();
    nearZero(0);
    nearZero(1);
    nearZero(0, 0);
    nearZero(0, 1);
    nearZero(0, 999999);
    nearZero(1, 0);
    nearZero(1, 1);
    nearZero(1, 999999);

    // The thread should have slept at least 5 miliseconds.
    // But certainly not more than 500 miliseconds.
    long present = System.currentTimeMillis();
    long diff = present - past;
    harness.debug("diff: " + diff);
    harness.check(diff > 5);
    harness.check(diff < 500);


    // A thread in interrupted state that goes to sleep gets
    // InterruptedException.
    harness.checkPoint("Interrupted state sleep");
    past = System.currentTimeMillis();
    interruptedSleep(0);
    interruptedSleep(1);
    interruptedSleep(5000);
    interruptedSleep(0, 0);
    interruptedSleep(1, 0);
    interruptedSleep(0, 1);
    interruptedSleep(1, 1);
    interruptedSleep(5000, 0);
    interruptedSleep(5000, 5000);

    // The thread should not actually have slept (much) since it was always
    // immediately waken up by the InterrupedException.
    present = System.currentTimeMillis();
    harness.check(present - past < 5000);
  }

  private void invalid(long milli)
  {
    boolean illegal_argument = false;
    try
      {
	Thread.sleep(milli);
      }
    catch (IllegalArgumentException iae)
      {
	illegal_argument = true;
      }
    catch(InterruptedException ie)
      {
	harness.debug("InterruptedException in invalid(" + milli + ")");
	harness.check(false);
      }
    harness.check(illegal_argument);
  }

  private void invalid(long milli, int nano)
  {
    boolean illegal_argument = false;
    try
      {
	Thread.sleep(milli, nano);
      }
    catch (IllegalArgumentException iae)
      {
	illegal_argument = true;
      }
    catch(InterruptedException ie)
      {
	harness.debug("InterruptedException in invalid("
		      + milli + ", " + nano + ")");
	harness.check(false);
      }
    harness.check(illegal_argument);

  }

  private void valid(long milli)
  {
    harness.checkPoint("valid long:" + milli);
    Thread helper = new Thread(this);
    helper_started = false;
    helper_done = false;
    helper_sleep = 1000;
    thread = Thread.currentThread();

    // Wait for the helper to start (and sleep immediately).
    helper.start();
    synchronized(this)
      {
	try
	  {
	    while (!helper_started)
	      this.wait();
	  }
	catch (InterruptedException ie)
	  {
	    harness.debug("Interrupted during helper start");
	    harness.check(false);
	  }
      }
    
    boolean interrupted_exception = false;
    try
      {
	Thread.sleep(milli);
      }
    catch (InterruptedException ie)
      {
	interrupted_exception = true;
      }
    harness.check(interrupted_exception);

    try
      {
	helper.join();
      }
    catch(InterruptedException ie)
      {
	harness.debug("Interruped during joining the helper thread");
	harness.check(false);
      }
    harness.check(helper_done);
  }

  private void valid(long milli, int nano)
  {
    harness.checkPoint("valid long " + milli + " int " + nano);
    Thread helper = new Thread(this);
    helper_started = false;
    helper_done = false;
    helper_sleep = 1000;
    thread = Thread.currentThread();
    
    // Wait for the helper to start (and sleep immediately).
    helper.start();
    synchronized(this)
      {
	try
	  {
	    while (!helper_started)
	      this.wait();
	  }
	catch (InterruptedException ie)
	  {
	    harness.debug("Interrupted during helper start");
	    harness.check(false);
	  }
      }
    
    boolean interrupted_exception = false;
    try
      {
	Thread.sleep(milli, nano);
      }
    catch (InterruptedException ie)
      {
	interrupted_exception = true;
      }
    catch (Exception x)
      {
        harness.debug(x);
        try
          {
            // wait for the interrupt from the helper
            Thread.sleep(1000);
          }
        catch (InterruptedException _)
          {
          }
      }
    harness.check(interrupted_exception);

    try
      {
	helper.join();
      }
    catch(InterruptedException ie)
      {
	harness.debug("Interrupted during joining the helper thread");
	harness.check(false);
      }
    harness.check(helper_done);
  }

  private void nearZero(long milli)
  {
    try
      {
	Thread.sleep(milli);
	harness.check(true);
      }
    catch(InterruptedException ie)
      {
	harness.debug("InterruptedException in nearZero(" + milli + ")");
	harness.check(false);
      }
  }

  private void nearZero(long milli, int nano)
  {
    try
      {
	Thread.sleep(milli, nano);
	harness.check(true);
      }
    catch(InterruptedException ie)
      {
	harness.debug("InterruptedException in nearZero("
		      + milli + ", " + nano + ")");
	harness.check(false);
      }
  }

  private void interruptedSleep(long milli)
  {
    boolean interrupted_exception = false;
    Thread.currentThread().interrupt();
    try
      {
	Thread.sleep(milli);
      }
    catch(InterruptedException ie)
      {
	interrupted_exception = true;
      }
    harness.check(interrupted_exception);
    harness.check(!Thread.interrupted());
  }

  private void interruptedSleep(long milli, int nano)
  {
    boolean interrupted_exception = false;
    Thread.currentThread().interrupt();
    try
      {
	Thread.sleep(milli, nano);
      }
    catch(InterruptedException ie)
      {
	interrupted_exception = true;
      }
    harness.check(interrupted_exception);
    harness.check(!Thread.interrupted());
  }
}

