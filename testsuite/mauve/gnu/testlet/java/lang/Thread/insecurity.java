// Copyright (C) 2006, 2007 Red Hat, Inc.
// Written by Gary Benson <gbenson@redhat.com>

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

// Tags: JDK1.2

package gnu.testlet.java.lang.Thread;

import java.security.Permission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class insecurity implements Testlet
{
  private static Permission[] noChecks = new Permission[0];

  public void test(TestHarness harness)
  {
    try {
      harness.checkPoint("setup");

      TestSecurityManager sm = new TestSecurityManager(harness);

      // The default SecurityManager.checkAccess(Thread) method should
      // only check permissions when the thread in question is a system
      // thread.  System threads are those whose parent is the system
      // threadgroup, which is the threadgroup with no parent.
      // 
      // The default SecurityManager.checkAccess(ThreadGroup) method
      // should only check permissions when the threadgroup in
      // question is the system threadgroup.
      ThreadGroup systemGroup = Thread.currentThread().getThreadGroup();
      while (systemGroup.getParent() != null)
	systemGroup = systemGroup.getParent();

      ThreadGroup nonSystemGroup = new ThreadGroup(systemGroup, "test group");

      Thread testThread = new Thread(nonSystemGroup, "test thread");
      harness.check(testThread.getThreadGroup().getParent() != null);

      Thread modifyGroupThread = new Thread(
	nonSystemGroup, new SysTestRunner(harness, sm));
      harness.check(modifyGroupThread.getThreadGroup().getParent() != null);

      Throwable threadDeath = new ThreadDeath();
      Throwable notThreadDeath = new ClassNotFoundException();

      Runnable runnable = new Runnable()
      {
	public void run()
	{
	}
      };

      Permission[] stopThread = new Permission[] {
	new RuntimePermission("stopThread")};

      try {
	sm.install();

	// corresponding throwpoint: java.lang.Thread-checkAccess
	harness.checkPoint("checkAccess");
	try {
	  sm.prepareChecks(noChecks);
	  testThread.checkAccess();
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// corresponding throwpoint: java.lang.Thread-interrupt
	harness.checkPoint("interrupt");
	try {
	  sm.prepareChecks(noChecks);
	  testThread.interrupt();
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// corresponding throwpoint: java.lang.Thread-suspend
	harness.checkPoint("suspend");
	try {
	  sm.prepareChecks(noChecks);
	  testThread.suspend();
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// corresponding throwpoint: java.lang.Thread-resume
	harness.checkPoint("resume");
	try {
	  sm.prepareChecks(noChecks);
	  testThread.resume();
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// corresponding throwpoint: java.lang.Thread-setPriority
	harness.checkPoint("setPriority");
	try {
	  int priority = testThread.getPriority();
	  sm.prepareChecks(noChecks);
	  testThread.setPriority(priority);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// corresponding throwpoint: java.lang.Thread-setName
	harness.checkPoint("setName");
	try {
	  sm.prepareChecks(noChecks);
	  testThread.setName("a test thread");
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// corresponding throwpoint: java.lang.Thread-setDaemon
	harness.checkPoint("setDaemon");
	try {
	  sm.prepareChecks(noChecks);
	  testThread.setDaemon(false);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// corresponding throwpoint: java.lang.Thread-stop()
	harness.checkPoint("stop()");
	try {
	  sm.prepareChecks(stopThread);
	  testThread.stop();
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// corresponding throwpoint: java.lang.Thread-stop(Throwable)
	harness.checkPoint("stop(Throwable)");
	try {
	  sm.prepareChecks(stopThread);
	  testThread.stop(threadDeath);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	try {
	  sm.prepareChecks(stopThread);
	  testThread.stop(notThreadDeath);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}
	
	// The noChecksGroup tests get run in a system thread.
	modifyGroupThread.start();
	modifyGroupThread.join();

	// corresponding throwpoints: java.lang.Thread-Thread(ThreadGroup, ...)
	harness.checkPoint("ThreadGroup constructors");
	for (int i = 1; i <= 4; i++) {
	  try {
	    sm.prepareChecks(noChecks);
	    switch (i) {
	    case 1:
	      new Thread(nonSystemGroup, runnable);
	      break;
	    case 2:
	      new Thread(nonSystemGroup, runnable, "test thread");
	      break;
	    case 3:
	      new Thread(nonSystemGroup, runnable, "test thread", 1024);
	      break;
	    case 4:
	      new Thread(nonSystemGroup, "test thread");
	      break;
	    }
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }
	}
      }
      finally {
	sm.uninstall();
      }
    }
    catch (Exception ex) {
      harness.debug(ex);
      harness.check(false, "Unexpected exception");
    }
  }

  // Stuff for the modifyThreadGroup tests
  public static class SysTestRunner implements Runnable
  {
    private TestHarness harness;
    private TestSecurityManager sm;

    private static Runnable runnable = new Runnable()
    {
      public void run()
      {
      }
    };

    public SysTestRunner(TestHarness harness, TestSecurityManager sm)
    {
      this.harness = harness;
      this.sm = sm;
    }

    public void run()
    {
      try {
	// corresponding throwpoint: java.lang.Thread-enumerate
	harness.checkPoint("enumerate");
	try {
	  sm.prepareChecks(noChecks);
	  Thread.enumerate(new Thread[0]);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}	

	// corresponding throwpoint: java.lang.Thread-Thread()
	// corresponding throwpoint: java.lang.Thread-Thread(Runnable)
	// corresponding throwpoint: java.lang.Thread-Thread(String)
	// corresponding throwpoint: java.lang.Thread-Thread(Runnable, String)
	harness.checkPoint("basic constructors");
	for (int i = 1; i <= 4; i++) {
	  try {
	    sm.prepareChecks(noChecks);
	    switch (i) {
	    case 1:
	      new Thread();
	      break;
	    case 2:
	      new Thread(runnable);
	      break;
	    case 3:
	      new Thread("test thread");
	      break;
	    case 4:
	      new Thread(runnable, "test thread");
	      break;
	    }
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }
	}
      }
      catch (Exception ex) {
	harness.debug(ex);
	harness.check(false, "Unexpected exception");
      }
    }
  }
}
