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

package gnu.testlet.java.net.ServerSocket;

import java.net.BindException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
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

      InetAddress inetaddr = InetAddress.getByName(null);
      String hostaddr = inetaddr.getHostAddress();

      ServerSocket ssocket = new ServerSocket(0, 50, inetaddr);
      int sport = ssocket.getLocalPort();
      Socket csocket = new Socket(inetaddr, sport, inetaddr, 0);
      int cport = csocket.getLocalPort();
      
      Permission[] checkListen80 = new Permission[] {
 	new SocketPermission("localhost:80", "listen")};

      Permission[] checkListen1024plus = new Permission[] {
 	new SocketPermission("localhost:1024-", "listen")};

      Permission[] checkAccept = new Permission[] {
 	new SocketPermission(hostaddr + ":" + cport, "accept")};

      Permission[] checkSelectorProvider = new Permission[] {
	new RuntimePermission("selectorProvider")};

      Permission[] checkSetFactory = new Permission[] {
	new RuntimePermission("setFactory")};
      
      TestSecurityManager sm = new TestSecurityManager(harness);
      try {
	sm.install();

	// throwpoint: java.net.ServerSocket-ServerSocket(int)
	harness.checkPoint("ServerSocket(int)");
	try {
	  sm.prepareChecks(checkListen80, checkSelectorProvider);
	  try {
	    new ServerSocket(80).close();
	  }
	  catch (BindException e) {
	  } 
	  sm.checkAllChecked();
	}
	catch (SecurityException e) {
	  harness.debug(e);
	  harness.check(false, "unexpected check");
	}
	try {
	  sm.prepareChecks(checkListen1024plus, checkSelectorProvider);
	  new ServerSocket(0).close();
	  sm.checkAllChecked();
	}
	catch (SecurityException e) {
	  harness.debug(e);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.net.ServerSocket-ServerSocket(int, int)
	harness.checkPoint("ServerSocket(int, int)");
	try {
	  sm.prepareChecks(checkListen80, checkSelectorProvider);
	  try {
	    new ServerSocket(80, 50).close();
	  }
	  catch (BindException e) {
	  } 
	  sm.checkAllChecked();
	}
	catch (SecurityException e) {
	  harness.debug(e);
	  harness.check(false, "unexpected check");
	}
	try {
	  sm.prepareChecks(checkListen1024plus, checkSelectorProvider);
	  new ServerSocket(0, 50).close();
	  sm.checkAllChecked();
	}
	catch (SecurityException e) {
	  harness.debug(e);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.net.ServerSocket-ServerSocket(int, int,InetAddress)
	harness.checkPoint("ServerSocket(int, int, InetAddress)");
	try {
	  sm.prepareChecks(checkListen80, checkSelectorProvider);
	  try {
	    new ServerSocket(80, 50, inetaddr).close();
	  }
	  catch (BindException e) {
	  } 
	  sm.checkAllChecked();
	}
	catch (SecurityException e) {
	  harness.debug(e);
	  harness.check(false, "unexpected check");
	}
	try {
	  sm.prepareChecks(checkListen1024plus, checkSelectorProvider);
	  new ServerSocket(0, 50, inetaddr).close();
	  sm.checkAllChecked();
	}
	catch (SecurityException e) {
	  harness.debug(e);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.net.ServerSocket-accept
	harness.checkPoint("accept");
	try {
	  sm.prepareChecks(checkAccept, checkSelectorProvider);
	  ssocket.accept().close();
	  sm.checkAllChecked();
	}
	catch (SecurityException e) {
	  harness.debug(e);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.net.ServerSocket-setSocketFactory
	harness.checkPoint("setSocketFactory");
	try {
	  sm.prepareHaltingChecks(checkSetFactory);
	  ServerSocket.setSocketFactory(null);
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
	csocket.close();
	ssocket.close();
      }
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false, "Unexpected exception");
    }
  }
}
