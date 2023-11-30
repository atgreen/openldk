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

package gnu.testlet.java.net.NetworkInterface;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketPermission;
import java.security.Permission;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.LinkedList;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class security implements Testlet
{
  public void test(TestHarness harness)
  {
    try {
      harness.checkPoint("setup");

      LinkedList list = new LinkedList();
      for (Enumeration e = NetworkInterface.getNetworkInterfaces();
	   e.hasMoreElements(); ) {
	NetworkInterface nif = (NetworkInterface) e.nextElement();
	for (Enumeration f = nif.getInetAddresses(); f.hasMoreElements(); )
	  list.add(f.nextElement());
      }
      harness.check(!list.isEmpty());

      Permission[] checks = new Permission[list.size()];
      for (int i = 0; i < list.size(); i++) {
	InetAddress addr = (InetAddress) list.get(i);
	checks[i] = new SocketPermission(addr.getHostAddress(), "resolve");
      }

      TestSecurityManager sm = new TestSecurityManager(harness);
      try {
	sm.install();

 	// throwpoint: java.net.NetworkInterface-getInetAddresses
	harness.checkPoint("getInetAddresses");
	try {
	  sm.prepareChecks(checks);
	  for (Enumeration e = NetworkInterface.getNetworkInterfaces();
	       e.hasMoreElements(); ) {
	    NetworkInterface nif = (NetworkInterface) e.nextElement();
	    nif.getInetAddresses();
	  }
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
