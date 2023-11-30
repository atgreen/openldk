/* AcceptGetLocalPort.java - Test for getLocalPort on accepted Socket.
   Copyright (C) 2005, Mark J. Wielaard  <mark@klomp.org>

This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA. */
   
// Tags: JDK1.0

package gnu.testlet.java.net.ServerSocket;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.net.*;
import java.io.*;

public class AcceptGetLocalPort implements Testlet, Runnable
{
  private static int port = 5678;

  public void test (TestHarness harness)
  {
    new Thread(this).start();
    try
      {
	ServerSocket ss = new ServerSocket(port);
	harness.check(ss.getLocalPort(), port);
	Socket s = ss.accept();
	harness.check(s.getLocalPort(), port);
	s.close();
	ss.close();
      }
    catch (IOException ioe)
      {
	harness.debug(ioe);
	harness.check(false, ioe.toString());
      }
  }

  public void run()
  {
    int i = 0;
    while (i < 10)
      {
	try
	  {
	    Socket s = new Socket("localhost", port);
	    break;
	  }
	catch (IOException ioe)
	  {
	    // ignore
	  }
	try
	  {
	    Thread.sleep(1000);
	  }
	catch (InterruptedException ie)
	  {
	    // ignore
	  }
      }
  }
}
