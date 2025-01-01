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

package gnu.testlet.java.net.SocketPermission;

import java.net.SocketPermission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class argument implements Testlet
{
  private Test[] hosts = new Test[] {
    new Test("", true),

    new Test("local:host", false),
    new Test("localhost", true),
    new Test("example.com", true),
    new Test("*.com", true), // XXX try wildcard in other positions

    new Test("209.132:177.50", false),
    new Test("209.132.177.50", true), // XXX try broken addresses

    new Test("[", false),
    new Test("[::192.9.5.5]3", false),
    new Test("[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]", true),
    new Test("[3ffe:2a00:100:7031::1]", true),
    new Test("[1080::8:800:200C:417A]", true),
    new Test("[::192.9.5.5]", true),
    new Test("[::FFFF:129.144.52.38]", true), // XXX try broken addresses

    new Test("FEDC:BA98:7654:3210:FEDC:BA98:7654:3210", true),
    new Test("3ffe:2a00:100:7031::1", false),
    new Test("1080::8:800:200C:417A", false),
    new Test("::192.9.5.5", false),
    new Test("::FFFF:129.144.52.38", false),
    new Test("0:0:0:0:0:0:0:1", true),
  };

  private Test[] ports = new Test[] {
    new Test("", true),
    new Test(":", true),   

    new Test(":80", true),
    new Test(":-80", true),
    new Test(":80-", true),
    new Test(":70-90", true),

    new Test(":8a", false),
    new Test(":-8a", false),
    new Test(":8a-", false),
    new Test(":7a-90", false),
    new Test(":70-9a", false),

    new Test(":800000", true),
    new Test(":-800000", true),
    new Test(":800000-", true),
    new Test(":700000-900000", true),

    new Test(":-", false),
    new Test(":--80", false),
    new Test(":-80-", false),
    new Test(":80--", false),
    new Test(":70--90", false),
    new Test(":-70-90", false),
    new Test(":-70--90", false),
    new Test(":70-90-", false),
    new Test(":-70-90-", false),
  };
  
  public void test(TestHarness harness)
  {
    harness.checkPoint("argument checking");
    
    for (int i = 0; i < hosts.length; i++) {
      for (int j = 0; j < ports.length; j++) {
	Test test = new Test(hosts[i], ports[j]);
	boolean success;

	try {
	  new SocketPermission(test.hostport, "connect");
	  success = true;
	}
	catch (IllegalArgumentException e) {
	  success = false;
	}

	harness.check(success == test.expect, test.hostport + " should "
		      + (test.expect ? "be ok" : "fail"));
      }
    }
  }

  private static class Test
  {
    String hostport;
    boolean expect;

    Test(String hostport, boolean expect)
    {
      this.hostport = hostport;
      this.expect = expect;
    }
    
    Test(Test host, Test port)
    {
      hostport = host.hostport + port.hostport;
      expect = host.expect && port.expect;
    }
  }
}
