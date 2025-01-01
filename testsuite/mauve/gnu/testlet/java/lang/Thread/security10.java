// Tags: JDK1.0

// Copyright (C) 2004, 2005 Free Software Foundation, Inc.
// Written by Michael Koch (konqueror@gmx.de)

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
import java.security.Permission;

public class security10 implements Testlet
{
  static class MySecurityManager extends SecurityManager
  {
    public void checkAccess(Thread thread)
    {
      throw new SecurityException();
    }
    public void checkAccess(ThreadGroup thread)
    {
      throw new SecurityException();
    }

    public void checkPermission(Permission perm)
    {
      RuntimePermission p = new RuntimePermission("setSecurityManager");
      if (p.implies(perm))
        return;
      super.checkPermission(perm);
    }
  }
  
  public void test (TestHarness h)
  {
    SecurityManager secman = System.getSecurityManager();
    try
      {
        testImpl(h);
      }
    finally
      {
        System.setSecurityManager(secman);
      }
  }

  private void testImpl(TestHarness h)
  {
    h.checkPoint("Thread creation");
    
    Thread testThread = new Thread();

    ThreadGroup group = new ThreadGroup("MyGroup");

    System.setSecurityManager(new MySecurityManager());
    
    Runnable run = new Runnable()
      {
	public void run()
	{
	  // Do nothing here.
	}
      };
    
    try
      {
	Thread thread = new Thread();
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }
    
    try
      {
	Thread thread = new Thread("MyThread");
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }
    
    try
      {
	Thread thread = new Thread(run);
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }
    
    try
      {
	Thread thread = new Thread(run, "MyThread");
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }

    h.checkPoint("Thread creation with ThreadGroup");
    
    try
      {
	Thread thread = new Thread(group, "MyThread");
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }

    try
      {
	Thread thread = new Thread(group, run);
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }

    try
      {
	Thread thread = new Thread(group, run, "MyThread");
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }

    try
      {
	Thread thread = new Thread(group, run, "MyThread", 0);
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }

    h.checkPoint("Thread.interrupt()");

    try
      {
	testThread.interrupt();
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }
    
    h.checkPoint("Thread.stop()");

    try
      {
	testThread.stop();
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }
    catch (UnsupportedOperationException e)
      {
	h.check(false);
      }

    // FIXME: Added testcase to let the Thread stop itself.
    
    h.checkPoint("Thread.stop(Throwable)");

    try
      {
	testThread.stop(new Error("Test"));
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }
    catch (UnsupportedOperationException e)
      {
	h.check(false);
      }
    
    // FIXME: Added testcase to let the Thread stop itself.

    h.checkPoint("Thread.suspend()");

    try
      {
	testThread.suspend();
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }
    
    h.checkPoint("Thread.resume()");

    try
      {
	testThread.resume();
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }
    
    h.checkPoint("Thread.setPriority(int)");

    try
      {
	testThread.setPriority(Thread.MIN_PRIORITY);
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }
    
    try
      {
	testThread.setPriority(Thread.MAX_PRIORITY);
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }
    
    h.checkPoint("Thread.setName(String)");

    try
      {
	testThread.setName("My-Thread");
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }
    
    h.checkPoint("Thread.enumerate(Thread[])");

    try
      {
        /**
         * does not call checkAccess(Thread) but
         * checkAccess(ThreadGroup)
         */
	Thread[] array = new Thread[1];
	Thread.enumerate(array);
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }
    
    h.checkPoint("Thread.setDaemon(boolean)");

    try
      {
	testThread.setDaemon(false);
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }
    
    try
      {
	testThread.setDaemon(true);
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }
    
    h.checkPoint("Thread.checkAccess()");

    try
      {
	testThread.checkAccess();
	h.check(false);
      }
    catch (SecurityException e)
      {
	h.check(true);
      }
  }
}

