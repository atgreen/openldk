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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.net.SocketPermission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class serialization implements Testlet
{
  private String[] hosts = new String[] {
    "",
    "localhost",
    "example.com",
    "*.com",
    "209.132.177.50",
    "[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]",
    "[3ffe:2a00:100:7031::1]",
    "[::192.9.5.5]",
  };

  private String[] ports = new String[] {
    "",
    ":",
    ":80",
    ":-80",
    ":80-",
    ":70-90",
    ":800000",
    ":700000-900000",
  };
  
  public void test(TestHarness harness)
  {
    harness.checkPoint("serialization checking");
    
    for (int i = 0; i < hosts.length; i++) {
      for (int j = 0; j < ports.length; j++) {
	for (int k = 1; k < 1 << actions.length; k++) {
	  SocketPermission p1 = new SocketPermission(
	    hosts[i] + ports[j], makeActions(k));
	  SocketPermission p2 = null;
	  try {
	    ByteArrayOutputStream buffer = new ByteArrayOutputStream();
	    ObjectOutput out = new ObjectOutputStream(buffer);
	    out.writeObject(p1);
	    out.close();

	    ObjectInput in = new ObjectInputStream(
	      new ByteArrayInputStream(buffer.toByteArray()));
	    p2 = (SocketPermission) in.readObject();
	    in.close();
	  }
	  catch (Exception e) {
	    harness.debug(e);
	  }
	  harness.check(p1.equals(p2));
	}
      }
    }
  }

  // stuff for actions checking
  private static String[] actions = {"accept", "connect", "listen", "resolve"};
  private static String makeActions(int mask)
  {
    String result = "";
    for (int i = 0; i < actions.length; i++) {
      if ((mask & (1 << i)) != 0) {
	if (result.length() > 0)
	  result += ",";
	result += actions[i];
      }
    }
    return result;
  }
}
