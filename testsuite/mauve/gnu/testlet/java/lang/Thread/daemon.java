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

public class daemon extends Thread implements Testlet
{
  TestHarness harness;

  boolean started = false;
  boolean please_stop = false;

  public void run()
  {
    synchronized(this)
      {
	started = true;
	notify();

	while (!please_stop) 
	  {
	    try 
	      { 
		wait(); 
	      } catch(InterruptedException ignored) { }
	  }
      }
  }

  public void test (TestHarness harness)
  {
    this.harness = harness;
    Thread current = Thread.currentThread();

    boolean status = current.isDaemon();
    boolean illegal_exception = false;
    try
      {
	current.setDaemon(!status);
      }
    catch (IllegalThreadStateException itse)
      {
	illegal_exception = true;
      }
    harness.check(illegal_exception,
		  "Cannot change daemon state on current Thread");
    harness.check(current.isDaemon() == status,
		  "Daemon status not changed when set on current Thread");
    
    daemon t = new daemon();
    harness.check(t.isDaemon() == status,
		  "Newly created thread gets daemon status of creator");
    
    t.setDaemon(!status);
    harness.check(t.isDaemon() != status,
		  "Can change daemon status on an unstarted Thread");
    status = t.isDaemon();


    // Make sure we have a running thread.
    t.start();
    synchronized(t)
      {
	while (!t.started)
	  try {
	    t.wait(); 
	  } 
	  catch (InterruptedException ignored) {
	  }
      }

    harness.check(t.isDaemon() == status,
		  "Daemon status does not change when starting a Thread");

    illegal_exception = false;
    try
      {
	t.setDaemon(!status);
      }
    catch (IllegalThreadStateException itse)
      {
	illegal_exception = true;
      }
    harness.check(illegal_exception,
		  "Cannot change daemon state on a running Thread");
    harness.check(t.isDaemon() == status,
		  "Daemon status not changed when set on a running Thread");
    status = t.isDaemon();
    
    // Make sure the thread exits
    synchronized(t)
      {
	t.please_stop = true;
	t.notify();
      }
    try { 
      t.join();
    }
    catch (InterruptedException ignored) { 
    }

    // Note: the Sun Javadoc seems to contradict itself on whether you can
    // change daemon state on an exitted Thread.  The observed behaviour
    // (on JDK 1.3.1 & 1.4.0) is that it works.  (But why would you bother?)
    illegal_exception = false;
    try
      {
	t.setDaemon(!status);
      }
    catch (IllegalThreadStateException itse)
      {
	illegal_exception = true;
      }
    harness.check(!illegal_exception,
		  "Can change daemon state on an exitted Thread");
    harness.check(t.isDaemon() != status,
		  "Daemon status changed when set on an exitted Thread");
  }
}
