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

public class interrupt implements Testlet
{

  public void test (TestHarness harness)
  {
    Thread current = Thread.currentThread();

    // Make sure interrupt flag is cleared
    Thread.interrupted();

    harness.check(!current.isInterrupted(),
		  "Thread.interrupted() makes isInterrupted() false");
    harness.check(!Thread.interrupted(),
		  "Thread.interrupted() makes interrupted() false");
    
    // Make sure interrupt flag is set
    current.interrupt();
    
    harness.check(current.isInterrupted(),
		  "interrupt() makes isInterrupted() true");
    harness.check(current.isInterrupted(),
		  "isInterrupt() doesn't clear interrupt flag");
    harness.check(Thread.interrupted(),
		  "interrupt() makes interrupted() true");
    
    // Set interrupt flag again for wait test
    current.interrupt();
    boolean interrupted_exception = false;
    try
      {
	Object o = new Object();
	synchronized(o) {o.wait(50);}
      }
    catch(InterruptedException ie)
      {
	interrupted_exception = true;
      }
    harness.check(interrupted_exception,
		  "wait with interrupt flag throws InterruptedException");
    
    harness.check(interrupted_exception && !Thread.interrupted(),
		  "InterruptedException in wait() clears interrupt flag");
    
    // Set interrupt flag again for sleep test
    current.interrupt();
    interrupted_exception = false;
    try
      {
	Thread.sleep(50);
      }
    catch(InterruptedException ie)
      {
	interrupted_exception = true;
      }
    harness.check(interrupted_exception,
		  "sleep with interrupt flag throws InterruptedException");
    
    harness.check(interrupted_exception && !Thread.interrupted(),
		  "InterruptedException in sleep() clears interrupt flag");

    // Set interrupt flag again for join test
    current.interrupt();
    interrupted_exception = false;
    try
      {
	current.join(50, 50);
      }
    catch(InterruptedException ie)
      {
	interrupted_exception = true;
      }
    harness.check(interrupted_exception,
		  "join with interrupt flag throws InterruptedException");
    
    harness.check(interrupted_exception && !Thread.interrupted(),
		  "InterruptedException in join() clears interrupt flag");
  }
}

