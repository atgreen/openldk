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

public class name implements Testlet
{

  public void test (TestHarness harness)
  {
    Thread current = Thread.currentThread();

    harness.check(current.getName() != null,
		    "Every Thread has a non-null name");

    Thread t  = new Thread("Test-Thread");
    harness.check(t.getName().equals("Test-Thread"),
		    "Create thread with name");

    t.setName("Test-Thread-NewName");
    harness.check(t.getName().equals("Test-Thread-NewName"),
		    "Setting new for Thread");

    t.start();
    t.setName("Test-Thread-NewName-Started");
    harness.check(t.getName().equals("Test-Thread-NewName-Started"),
		    "Setting new name for started Thread");

    boolean null_exception = false;
    try
      {
	new Thread((String)null);
      }
    catch (NullPointerException npe)
      {
	null_exception = true;
      }
    harness.check(null_exception, "Cannot create Thread with null name");

    t = new Thread();
    String name = t.getName();
    harness.check(name != null, "New Thread has non-null name");

    null_exception = false;
    try
      {
	t.setName(null);
      }
    catch (NullPointerException npe)
      {
	null_exception = true;
      }
    harness.check(null_exception, "Cannot set Thread name to null");

    harness.check(t.getName().equals(name),
		    "Setting Thread name to null doesn't change name");
  }
}

