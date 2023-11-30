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

public class isAlive extends Thread implements Testlet
{

  boolean started = false;
  boolean please_stop = false;

  public void run()
  {
    synchronized(this)
      {
	started = true;
	notify();
	while(!please_stop)
	  try { this.wait(); } catch (InterruptedException ignore) { }
      }
  }

  public void test (TestHarness harness)
  {
    Thread current = Thread.currentThread();

    boolean alive = current.isAlive();
    harness.check(alive, "Current running thread is always alive");

    isAlive t  = new isAlive();
    harness.check(!t.isAlive(), "Newly created threads are not alive");

    t.start();
    synchronized(t)
      {
	while (!t.started)
	  try { t.wait(); } catch (InterruptedException ignore) { }

	harness.check(t.isAlive(), "Running threads are alive");

	t.please_stop = true;
	t.notify();
      }
    try { t.join(); } catch (InterruptedException ignore) { }

    harness.check(!t.isAlive(), "Stopped threads are not alive");
  }
}

