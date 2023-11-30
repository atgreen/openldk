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

package gnu.testlet.java.lang.ThreadGroup;

import java.security.Permission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class insecurity implements Testlet
{
  public void test(TestHarness harness)
  {
    try {
      harness.checkPoint("setup");

      // The default SecurityManager.checkAccess(ThreadGroup) method
      // should only check permissions when the threadgroup in
      // question is the system threadgroup, which is defined as the
      // threadgroup with no parent.

      ThreadGroup systemGroup = Thread.currentThread().getThreadGroup();
      while (systemGroup.getParent() != null)
	systemGroup = systemGroup.getParent();

      ThreadGroup nonSystemGroup = new ThreadGroup(systemGroup, "test group");

      Thread testThread = new Thread(nonSystemGroup, new TestRunner(harness));

      testThread.start();
      testThread.join();
    }
    catch (Exception ex) {
      harness.debug(ex);
      harness.check(false, "Unexpected exception");
    }
  }
  
  public static class TestRunner implements Runnable
  {
    private TestHarness harness;

    public TestRunner(TestHarness harness)
    {
      this.harness = harness;
    }

    public void run()
    {
      try {
	ThreadGroup testGroup = Thread.currentThread().getThreadGroup();
	harness.check(testGroup.getParent() != null);

	ThreadGroup nonSystemGroup = new ThreadGroup(testGroup, "test group");
	harness.check(nonSystemGroup.getParent() != null);
	
	Permission[] noChecks = new Permission[0];

	TestSecurityManager sm = new TestSecurityManager(harness);
	try {
	  sm.install();

	  // corresponding throwpoint:
	  // java.lang.ThreadGroup-ThreadGroup(String)
	  harness.checkPoint("ThreadGroup(String)");
	  try {
	    sm.prepareChecks(noChecks);
	    new ThreadGroup("test");
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }
	
	  // corresponding throwpoint:
	  // java.lang.ThreadGroup-ThreadGroup(ThreadGroup, String)
	  harness.checkPoint("ThreadGroup(ThreadGroup, String)");
	  try {
	    sm.prepareChecks(noChecks);
	    new ThreadGroup(testGroup, "test");
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }

	  // corresponding throwpoint: java.lang.ThreadGroup-checkAccess
	  harness.checkPoint("checkAccess");
	  try {
	    sm.prepareChecks(noChecks);
	    testGroup.checkAccess();
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }

	  // corresponding throwpoint:
	  // java.lang.ThreadGroup-enumerate(Thread[])
	  harness.checkPoint("enumerate(Thread[])");
	  try {
	    sm.prepareChecks(noChecks);
	    testGroup.enumerate(new Thread[0]);
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }	

	  // corresponding throwpoint:
	  // java.lang.ThreadGroup-enumerate(Thread[], boolean)
	  harness.checkPoint("enumerate(Thread[], boolean)");
	  for (int i = 0; i <= 1; i++) {
	    try {
	      sm.prepareChecks(noChecks);
	      testGroup.enumerate(new Thread[0], i == 1);
	      sm.checkAllChecked();
	    }
	    catch (SecurityException ex) {
	      harness.debug(ex);
	      harness.check(false, "unexpected check");
	    }	
	  }

	  // corresponding throwpoint:
	  // java.lang.ThreadGroup-enumerate(ThreadGroup[])
	  harness.checkPoint("enumerate(ThreadGroup[])");
	  try {
	    sm.prepareChecks(noChecks);
	    testGroup.enumerate(new ThreadGroup[0]);
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }	

	  // corresponding throwpoint:
	  // java.lang.ThreadGroup-enumerate(ThreadGroup[], boolean)
	  harness.checkPoint("enumerate(ThreadGroup[], boolean)");
	  for (int i = 0; i <= 1; i++) {
	    try {
	      sm.prepareChecks(noChecks);
	      testGroup.enumerate(new ThreadGroup[0], i == 1);
	      sm.checkAllChecked();
	    }
	    catch (SecurityException ex) {
	      harness.debug(ex);
	      harness.check(false, "unexpected check");
	    }	
	  }

	  // corresponding throwpoint: java.lang.ThreadGroup-getParent
	  harness.checkPoint("getParent");
	  try {
	    sm.prepareChecks(noChecks);
	    nonSystemGroup.getParent();
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }
	
	  // corresponding throwpoint: java.lang.ThreadGroup-setDaemon
	  harness.checkPoint("setDaemon");
	  try {
	    sm.prepareChecks(noChecks);
	    testGroup.setDaemon(false);
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }
	
	  // corresponding throwpoint: java.lang.ThreadGroup-setMaxPriority
	  harness.checkPoint("setMaxPriority");
	  try {
	    int priority = testGroup.getMaxPriority();
	    sm.prepareChecks(noChecks);
	    testGroup.setMaxPriority(priority);
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }

	  // corresponding throwpoint: java.lang.ThreadGroup-suspend
	  harness.checkPoint("suspend");
	  try {
	    sm.prepareChecks(noChecks);
	    nonSystemGroup.suspend();
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }

	  // corresponding throwpoint: java.lang.ThreadGroup-resume
	  harness.checkPoint("resume");
	  try {
	    sm.prepareChecks(noChecks);
	    nonSystemGroup.resume();
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }

	  // corresponding throwpoint: java.lang.ThreadGroup-interrupt
	  harness.checkPoint("interrupt");
	  try {
	    sm.prepareChecks(noChecks);
	    nonSystemGroup.interrupt();
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }

	  // corresponding throwpoint: java.lang.ThreadGroup-stop
	  harness.checkPoint("stop");
	  try {
	    sm.prepareChecks(noChecks);
	    nonSystemGroup.stop();
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
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
  }
}
