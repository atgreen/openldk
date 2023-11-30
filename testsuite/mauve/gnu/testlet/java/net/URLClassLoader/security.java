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

package gnu.testlet.java.net.URLClassLoader;

import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.net.URLStreamHandlerFactory;
import java.security.Permission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class security implements Testlet
{
  public void test(TestHarness harness)
  {
    try {
      ClassLoader loader = getClass().getClassLoader();
      URLStreamHandlerFactory ushf = new TestUSHFactory();
      
      Permission[] createClassLoader = new Permission[] {
	new RuntimePermission("createClassLoader")};

      TestSecurityManager sm = new TestSecurityManager(harness);
      try {
	sm.install();

	// throwpoint: java.net.URLClassLoader-URLClassLoader(URL[])
	harness.checkPoint("Constructor (1 arg)");
	try {
	  sm.prepareChecks(createClassLoader);
	  new URLClassLoader(new URL[0]);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.net.URLClassLoader-URLClassLoader(URL[], ClassLoader)
	harness.checkPoint("Constructor (2 arg)");
	try {
	  sm.prepareChecks(createClassLoader);
	  new URLClassLoader(new URL[0], loader);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.net.URLClassLoader-URLClassLoader(URL[], ClassLoader, URLStreamHandlerFactory)
	harness.checkPoint("Constructor (3 arg)");
	try {
	  sm.prepareChecks(createClassLoader);
	  new URLClassLoader(new URL[0], loader, ushf);
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

  private static class TestUSHFactory implements URLStreamHandlerFactory
  {
    public URLStreamHandler createURLStreamHandler(String protocol)
    {
      return new URLStreamHandler()
        {
          protected URLConnection openConnection(URL u) throws IOException
          {
            throw new RuntimeException("not implemented");
          }
        };
    }
  }
}
