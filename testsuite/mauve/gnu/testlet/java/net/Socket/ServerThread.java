// Tags: not-a-test

/*
   Copyright (C) 2005 Free Software Foundation

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
import gnu.testlet.TestHarness;
import java.net.*;
import java.io.*;

/* Implements a simple server socket listening on a port. 
Socket tests can connect to this port. */

class ServerThread extends Thread 
{
  ServerSocket sock;
  TestHarness harness;

  public ServerThread(TestHarness harness)
  {
    this(harness, 14610);
  }

  public ServerThread(TestHarness harness, int port)
  { 
    this.harness = harness;
    try
    {
      sock = new ServerSocket(port);
      this.start();
    }
    catch (IOException x)
    {
      harness.fail(x.toString());
    }
  }
  
  public void close()
  {
    try
    {
      sock.close();
    }
    catch (IOException x)
    {
      harness.fail(x.toString());
    }
  }
  
  public void run()
  {
    try
    {
    while (true)
      {
	Socket s = sock.accept();
	InputStream is = s.getInputStream();
	byte[] data = new byte[512];
	boolean done = false;
	while (!done)
	  {
	    if (is.read(data, 0, data.length) < 0)
	      done = true;
	  }
      }
    }
    catch (IOException x)
    {
      // Ignored
    }
  }
}
