// Tags: GUI JDK1.3

// Copyright (C) 2006 Red Hat, Inc.
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

package gnu.testlet.java.awt.Toolkit;

import java.awt.AWTEvent;
import java.awt.AWTPermission;
import java.awt.Frame;
import java.awt.JobAttributes;
import java.awt.PageAttributes;
import java.awt.Toolkit;
import java.awt.event.AWTEventListener;
import java.security.Permission;
import java.util.Properties;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class security implements Testlet
{
  public void test(TestHarness harness)
  {
    try {
      Toolkit toolkit = Toolkit.getDefaultToolkit();
      toolkit.getSystemEventQueue();

      AWTEventListener listener = new TestEventListener();

      Frame frame = new Frame();
      Properties props = new Properties();
      JobAttributes jobattrs = new JobAttributes();
      PageAttributes pageattrs = new PageAttributes();

      Permission[] listenToAllAWTEvents = new Permission[] {
	new AWTPermission("listenToAllAWTEvents")};

      Permission[] queuePrintJob = new Permission[] {
	new RuntimePermission("queuePrintJob")};

      Permission[] accessClipboard = new Permission[] {
	new AWTPermission("accessClipboard")};

      Permission[] accessEventQueue = new Permission[] {
	new AWTPermission("accessEventQueue")};

      TestSecurityManager sm = new TestSecurityManager(harness);
      try {
	sm.install();

	// throwpoint: java.awt.Toolkit-addAWTEventListener
	harness.checkPoint("addAWTEventListener");
	try {
	  sm.prepareChecks(listenToAllAWTEvents);
	  toolkit.addAWTEventListener(listener, AWTEvent.KEY_EVENT_MASK);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.awt.Toolkit-removeAWTEventListener
	harness.checkPoint("removeAWTEventListener");
	try {
	  sm.prepareChecks(listenToAllAWTEvents);
	  toolkit.removeAWTEventListener(listener);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.awt.Toolkit-getPrintJob(Frame, String, Properties)
	harness.checkPoint("getPrintJob(3-arg)");
	try {
	  sm.prepareHaltingChecks(queuePrintJob);
	  toolkit.getPrintJob(frame, "Test job", props);
	  harness.check(false);	  
	}
	catch (TestSecurityManager.SuccessException ex) {
	  harness.check(true);
	} 
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.awt.Toolkit-getPrintJob(Frame, String, JobAttributes, PageAttributes)
	harness.checkPoint("getPrintJob(4-arg)");
	try {
	  sm.prepareHaltingChecks(queuePrintJob);
	  toolkit.getPrintJob(frame, "Test job", jobattrs, pageattrs);
	  harness.check(false);	  
	}
	catch (TestSecurityManager.SuccessException ex) {
	  harness.check(true);
	} 
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.awt.Toolkit-getSystemClipboard
	harness.checkPoint("getSystemClipboard");
	try {
	  sm.prepareChecks(accessClipboard);
	  toolkit.getSystemClipboard();
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.awt.Toolkit-getSystemEventQueue
	harness.checkPoint("getSystemEventQueue");
	try {
	  sm.prepareChecks(accessEventQueue);
	  toolkit.getSystemEventQueue();
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

  private static class TestEventListener implements AWTEventListener
  {
    public void eventDispatched(AWTEvent event)
    {
    }
  }
}
