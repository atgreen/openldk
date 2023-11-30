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

package gnu.testlet.java.net.Socket;

import java.net.InetAddress;
import java.net.InetSocketAddress;
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
    try
    {

      harness.checkPoint("setup");

      InetAddress inetaddr = InetAddress.getByName(null);
      String hostname = inetaddr.getHostName();
      String hostaddr = inetaddr.getHostAddress();
      harness.check(!hostname.equals(hostaddr));

      ServerSocket socket = new ServerSocket(0, 50, inetaddr);
      int hostport = socket.getLocalPort();

      InetSocketAddress sockaddr = new InetSocketAddress(inetaddr, hostport);

      Permission[] checkConnect = new Permission[] {
        new SocketPermission(hostaddr + ":" + hostport, "connect")};

      Permission[] checkResolveConnect = new Permission[] {
        new SocketPermission(hostname, "resolve"),
        new SocketPermission(hostaddr + ":" + hostport, "connect")};

      Permission[] checkSelectorProvider = new Permission[] {
        new RuntimePermission("selectorProvider")};

      Permission[] checkSetFactory = new Permission[] {
        new RuntimePermission("setFactory")};

      TestSecurityManager sm = new TestSecurityManager(harness);
      try {
        sm.install();

        // throwpoint: java.net.Socket-Socket(InetAddress, int)
        try {
          harness.checkPoint("Socket(InetAddress, int)");
          sm.prepareChecks(checkConnect, checkSelectorProvider);
          new Socket(inetaddr, hostport).close();
          sm.checkAllChecked();
        }
        catch (SecurityException e) {
          harness.debug(e);
          harness.check(false, "unexpected check");
        }

        // throwpoint: java.net.Socket-Socket(String, int)
        harness.checkPoint("Socket(String, int)");
        try {
          sm.prepareChecks(checkConnect, checkSelectorProvider);
          new Socket(hostaddr, hostport).close();
          sm.checkAllChecked();
        }
        catch (SecurityException e) {
          harness.debug(e);
          harness.check(false, "unexpected check");
        }
        try {
          sm.prepareChecks(checkResolveConnect, checkSelectorProvider);
          new Socket(hostname, hostport).close();
          sm.checkAllChecked();
        }
        catch (SecurityException e) {
          harness.debug(e);
          harness.check(false, "unexpected check");
        }

        // throwpoint: java.net.Socket-Socket(InetAddress, int, boolean)
        harness.checkPoint("Socket(InetAddress, int, boolean)");
        for (int i = 0; i < 2; i++) {
          try {
            sm.prepareChecks(checkConnect, checkSelectorProvider);
            new Socket(inetaddr, hostport, i == 0).close();
            sm.checkAllChecked();
          }
          catch (SecurityException e) {
            harness.debug(e);
            harness.check(false, "unexpected check");
          }
        }

        // throwpoint: java.net.Socket-Socket(String, int, boolean)
        harness.checkPoint("Socket(String, int, boolean)");
        for (int i = 0; i < 2; i++) {
          try {
            sm.prepareChecks(checkConnect, checkSelectorProvider);
            new Socket(hostaddr, hostport, i == 0).close();
            sm.checkAllChecked();
          }
          catch (SecurityException e) {
            harness.debug(e);
            harness.check(false, "unexpected check");
          }
          try {
            sm.prepareChecks(checkResolveConnect, checkSelectorProvider);
            new Socket(hostname, hostport, i == 0).close();
            sm.checkAllChecked();
          }
          catch (SecurityException e) {
            harness.debug(e);
            harness.check(false, "unexpected check");
          }
        }

        // throwpoint: java.net.Socket-Socket(InetAddress,int,InetAddress,int)
        harness.checkPoint("Socket(InetAddress, int, InetAddress, int)");
        try {
          sm.prepareChecks(checkConnect, checkSelectorProvider);
          new Socket(inetaddr, hostport, inetaddr, 0).close();
          sm.checkAllChecked();
        }
        catch (SecurityException e) {
          harness.debug(e);
          harness.check(false, "unexpected check");
        }

        // throwpoint: java.net.Socket-Socket(String, int, InetAddress, int)
        harness.checkPoint("Socket(String, int, InetAddress, int)");
        try {
          sm.prepareChecks(checkConnect, checkSelectorProvider);
          new Socket(hostaddr, hostport, inetaddr, 0).close();
          sm.checkAllChecked();
        }
        catch (SecurityException e) {
          harness.debug(e);
          harness.check(false, "unexpected check");
        }
        try {
          sm.prepareChecks(checkResolveConnect, checkSelectorProvider);
          new Socket(hostname, hostport, inetaddr, 0).close();
          sm.checkAllChecked();
        }
        catch (SecurityException e) {
          harness.debug(e);
          harness.check(false, "unexpected check");
        }

        // throwpoint: TODO: java.net.Socket-Socket(Proxy)

        // throwpoint: java.net.Socket-connect(InetSocketAddress)
        harness.checkPoint("connect(InetSocketAddress)");
        try {
          sm.prepareChecks(checkConnect, checkSelectorProvider);
          Socket sock = new Socket();
          sock.connect(sockaddr, hostport);
          sock.close();
          sm.checkAllChecked();
        }
        catch (SecurityException e) {
          harness.debug(e);
          harness.check(false, "unexpected check");
        }

        // throwpoint: java.net.Socket-setSocketImplFactory
        harness.checkPoint("setSocketImplFactory");
        try {
          sm.prepareHaltingChecks(checkSetFactory);
          Socket.setSocketImplFactory(null);
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
    catch (Exception e) {
      harness.debug(e);
      harness.check(false, "Unexpected exception");
    }
  }
}

