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

package gnu.testlet.java.net.InetAddress;

import java.net.InetAddress;
import java.net.SocketPermission;
import java.security.Permission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class security implements Testlet
{
  public void test(TestHarness harness)
  {
    try {
      harness.checkPoint("setup");

      InetAddress localhost = InetAddress.getLocalHost();
      String hostname = localhost.getHostName();
      harness.check(!hostname.equals(localhost.getHostAddress()));
      byte[] hostaddr = localhost.getAddress();

      Permission[] checkConnect = new Permission[] {
	new SocketPermission(hostname, "resolve")};
      
      TestSecurityManager sm = new TestSecurityManager(harness);
      try {
	sm.install();

 	// throwpoint: java.net.InetAddress-getHostName
	harness.checkPoint("getHostName");
	try {
	  sm.prepareChecks(checkConnect);
	  InetAddress.getByAddress(hostaddr).getHostName();
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

 	// throwpoint: java.net.InetAddress-getCanonicalHostName
	harness.checkPoint("getCanonicalHostName");
	try {
	  sm.prepareChecks(checkConnect);
	  InetAddress.getByAddress(hostaddr).getCanonicalHostName();
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

 	// throwpoint: java.net.InetAddress-getByName
	harness.checkPoint("getByName");
	try {
	  sm.prepareChecks(checkConnect);
	  InetAddress.getByName(hostname);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.net.InetAddress-getAllByName
	harness.checkPoint("getAllByName");
	try {
	  sm.prepareChecks(checkConnect);
	  InetAddress.getAllByName(hostname);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.net.InetAddress-getLocalHost
	harness.checkPoint("getLocalHost");
	try {
	  sm.prepareChecks(checkConnect);
	  InetAddress.getLocalHost();
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
