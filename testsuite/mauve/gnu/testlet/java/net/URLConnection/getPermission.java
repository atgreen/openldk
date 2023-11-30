// Tags: JDK1.2

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Mark Wielaard (mark@klomp.org)

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.net.URLConnection;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.io.*;
import java.net.*;
import java.security.*;

public class getPermission extends URLConnection implements Testlet
{
  public void test (TestHarness harness)
  {
    // If not overridden then AllPermission
    try
      {
	Permission p = this.getPermission();
	harness.check(p, new AllPermission());
      }
    catch (IOException ioe)
      {
	harness.debug(ioe);
	harness.check(false);
      }

    // File needs at least read permission
    try
      {
	String file = "dummy";
	URL u = new URL("file:" + file);
	URLConnection uc = u.openConnection();
	Permission p = uc.getPermission();
	harness.check(p, new FilePermission(file, "read"));
      }
    catch (IOException ioe)
      {
	harness.check(false);
      }

    // File needs at least read permission to the absolute file
    try
      {
	String file = "file";
	File f = new File(file);
	URL u = f.toURL();
	URLConnection uc = u.openConnection();
	Permission p = uc.getPermission();
	harness.check(p, new FilePermission(f.getAbsolutePath(), "read"));
      }
    catch (IOException ioe)
      {
	harness.debug(ioe);
	harness.check(false);
      }

    // HTTP needs at least connect permission
    try
      {
	String host = "dummy";
	int port = 80;
	URL u = new URL("http://" + host + "/");
	URLConnection uc = u.openConnection();
	Permission p = uc.getPermission();
	harness.check(p, new SocketPermission(host + ":" + port, "connect"));
      }
    catch (IOException ioe)
      {
	harness.debug(ioe);
	harness.check(false);
      }

    // HTTP on non-standard port needs at least connect permission on that port
    try
      {
	String host = "dummy";
	int port = 667;
	URL u = new URL("http://" + host + ":" + port + "/");
	URLConnection uc = u.openConnection();
	Permission p = uc.getPermission();
	harness.check(p, new SocketPermission(host + ":" + port, "connect"));
      }
    catch (IOException ioe)
      {
	harness.debug(ioe);
	harness.check(false);
      }
  }

  // Dummy constructor
  public getPermission() throws IOException
  {
    super(new URL("file:dummy"));
  }
  
  // Dummy connect
  public void connect()
  {
    connected = true;
  }
}
