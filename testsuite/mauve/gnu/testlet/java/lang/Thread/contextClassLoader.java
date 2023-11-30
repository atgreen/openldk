// Tags: JDK1.2

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

public class contextClassLoader implements Testlet, Runnable
{

  // ClassLoader which should be returned by getContextClassLoader in run().
  ClassLoader checkClassLoader;
  String check_msg;
  TestHarness h;

  public void test (TestHarness harness)
  {
    h = harness;

    Thread current = Thread.currentThread();
    ClassLoader current_cl = current.getContextClassLoader();
    try
      {
	ClassLoader system_cl = ClassLoader.getSystemClassLoader();
	harness.check(current_cl, system_cl,
		      "Default contextClassLoader is System ClassLoader");
	
	Thread t  = new Thread(this, "CL-Test-Thread");
	ClassLoader t_cl = t.getContextClassLoader();
	harness.check(t_cl, current_cl, "New thread inherits classloader");
	checkClassLoader = t_cl;
	check_msg = "Run with default contextClassLoader";
	t.start();
	try
	  {
	    t.join();
	  }
	catch (InterruptedException e)
	  {
	    throw new Error(e);
	  }
	
	current.setContextClassLoader(null);
	harness.check(current.getContextClassLoader() == null,
		      "null is a valid contextClassLoader");
	
	t = new Thread(this, "CL-Test-Thread-2");
	harness.check(t.getContextClassLoader(), null,
		      "New thread inherits null classloader");
	checkClassLoader = null;
	check_msg = "run with null classloader";
	t.start();
	try
	  {
	    t.join();
	  }
	catch (InterruptedException e)
	  {
	    throw new Error(e);
	  }
      }
    finally
      {
	// And set back the current classloader
	current.setContextClassLoader(current_cl);

	harness.check(current.getContextClassLoader(), current_cl,
		      "Reset context classloader");
      }
  }

  public void run()
  {
    Thread current = Thread.currentThread();
    ClassLoader cl = current.getContextClassLoader();
    h.debug("checkClassLoader: " + checkClassLoader);
    h.debug(current + ": " + cl);
    h.check(cl, checkClassLoader, check_msg);
  }
}

