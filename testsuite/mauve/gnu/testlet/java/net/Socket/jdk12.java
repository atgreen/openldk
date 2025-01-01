// Tags: JDK1.2
// Uses: ServerThread

/*
  Copyright (C) 2003 C. Brian Jones
  Copyright (C) 2005 Mark J. Wielaard
  
  This file is part of Mauve.
  
  Mauve is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.
  
  Mauve is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with Mauve; see the file COPYING.  If not, write to
  the Free Software Foundation, 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.
*/

package gnu.testlet.java.net.Socket;

import java.io.IOException;
import java.net.*;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class jdk12 implements Testlet
{
  public void test (TestHarness harness)
  {
    String host = "localhost";
    int port = 14610;
    Socket sock = null;
    ServerThread server = new ServerThread(harness, port);
    try 
      {
	sock = new Socket (host, port);

	harness.checkPoint("setSendBufferSize()");
	harness.checkPoint("getSendBufferSize()");
	harness.checkPoint("setReceiveBufferSize()");
	harness.checkPoint("getReceiveBufferSize()");

      }
    catch (Throwable t)
      { 
	harness.debug (t);
	harness.fail ("unexpected error: " + t.getMessage ());
      }
    finally {
      try {
	if (sock != null)
	  sock.close();
	server.close();
      } catch(IOException ignored) {}
    }
  }
}
