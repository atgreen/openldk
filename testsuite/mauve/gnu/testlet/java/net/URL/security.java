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

package gnu.testlet.java.net.URL;

import java.io.IOException;
import java.net.NetPermission;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.security.Permission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class security implements Testlet
{
  public void test(TestHarness harness)
  {
    try {
      URLStreamHandler handler = new TestURLStreamHandler();

      URL context = new URL("http://www.redhat.com/");
      
      Permission[] specifyStreamHandler = new Permission[] {
	new NetPermission("specifyStreamHandler")};

      Permission[] checkSetFactory = new Permission[] {
	new RuntimePermission("setFactory")};

      TestSecurityManager sm = new TestSecurityManager(harness);
      try {
	sm.install();

	// throwpoint: java.net.URL-URL(String, String, int, String, URLStreamHandler)
	harness.checkPoint("URL(String, String, int, String, URLStreamHandler)");
	try {
	  sm.prepareChecks(specifyStreamHandler);
	  new URL("redhat", "redhat.com", 23, "/red/hat/", handler);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.net.URL-URL(URL, String, URLStreamHandler)
	harness.checkPoint("URL(String, String, int, String, URLStreamHandler)");
	try {
	  sm.prepareChecks(specifyStreamHandler);
	  new URL(context, "/red/hat/", handler);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.net.URL-setURLStreamHandlerFactory
	harness.checkPoint("setURLStreamHandlerFactory");
	try {
	  sm.prepareHaltingChecks(checkSetFactory);
	  URL.setURLStreamHandlerFactory(null);
	  harness.check(false);	  
	}
	catch (TestSecurityManager.SuccessException ex) {
	  harness.check(true);
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

  private static class TestURLStreamHandler extends URLStreamHandler
  {
    public URLConnection openConnection(URL url) throws IOException
    {
      throw new RuntimeException("not implemented");
    }
  }
}
