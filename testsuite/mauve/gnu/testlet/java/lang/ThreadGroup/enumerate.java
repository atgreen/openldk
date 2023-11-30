// Tags: JDK1.0

// Copyright (C) 2001 Red Hat, Inc.

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

package gnu.testlet.java.lang.ThreadGroup;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class enumerate extends Thread implements Testlet
{
  public enumerate (ThreadGroup g, String name)
  {
    super (g, name);
  }

  public enumerate ()
  {
  }

  public void test (TestHarness harness)
  {
    ThreadGroup here = Thread.currentThread ().getThreadGroup ();
    ThreadGroup group = new ThreadGroup (here, "enumtestgroup");
    ThreadGroup group2 = new ThreadGroup (group, "enumsubgroup");

    enumerate e = new enumerate (group, "name1");
    e = new enumerate (group2, "name2");

    int thread_count = group.activeCount () + 10;
    Thread[] thread_list = new Thread[thread_count];
    thread_count = group.enumerate (thread_list, true);

    // There aren't any active threads since we never started E.
    harness.check (thread_count, 0);
  }
}
