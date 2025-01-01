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

public class getThreadGroup implements Testlet
{
  public void test (TestHarness harness)
  {
    Thread current = Thread.currentThread();
    harness.check(current.getThreadGroup() != null,
		    "Every Thread has a ThreadGroup");

    // By default the group is the same as the one from current thread
    Thread t = new Thread();
    harness.check(t.getThreadGroup(), current.getThreadGroup());

    // After the Thread dies the group should be null
    t.start();
    try { t.join(); } catch (InterruptedException ignored) { }
    harness.check(t.getThreadGroup(), null);

    // Explicitly set the ThreadGroup
    ThreadGroup g = new ThreadGroup("Test-Group");
    t = new Thread(g, "Test-Thread");
    harness.check(t.getThreadGroup(), g);

    // Null Thread group means same as the current Thread
    t = new Thread((ThreadGroup) null, "Test-Thread-2");
    harness.check(t.getThreadGroup(), current.getThreadGroup());

    // Except when a Security Manager is set, but we don't want to set
    // that here because that might interfere with other tests...

  }
}
