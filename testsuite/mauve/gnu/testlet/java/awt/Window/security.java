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

package gnu.testlet.java.awt.Window;

import java.awt.AWTPermission;
import java.awt.Frame;
import java.awt.GraphicsConfiguration;
import java.awt.Window;
import java.security.Permission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class security implements Testlet
{
  public void test(TestHarness harness)
  {
    try {
      Frame frame = new Frame();
      Window window = new Window(frame);
      GraphicsConfiguration gc = window.getGraphicsConfiguration();

      Permission[] showWindowWithoutWarningBanner = new Permission[] {
	new AWTPermission("showWindowWithoutWarningBanner")};

      TestSecurityManager sm = new TestSecurityManager(harness);
      try {
	sm.install();

	// throwpoint: java.awt.Window-Window(Frame)
	harness.checkPoint("Window(Frame)");
	try {
	  sm.prepareChecks(showWindowWithoutWarningBanner);
	  new Window(frame);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.awt.Window-Window(Window)
	harness.checkPoint("Window(Window)");
	try {
	  sm.prepareChecks(showWindowWithoutWarningBanner);
	  new Window(window);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.awt.Window-Window(Window, GraphicsConfiguration)
	harness.checkPoint("Window(Window, GraphicsConfiguration)");
	try {
	  sm.prepareChecks(showWindowWithoutWarningBanner);
	  new Window(window, gc);
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
