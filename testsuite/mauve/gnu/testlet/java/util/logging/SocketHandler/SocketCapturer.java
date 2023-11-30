// Tags: not-a-test

// Copyright (C) 2004 Sascha Brawer <brawer@dandelis.ch>

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

package gnu.testlet.java.util.logging.SocketHandler;

import java.io.*;
import java.net.*;


/**
 * A helper class that opens a TCP/IP server socket and starts
 * a separate thread which listens for a connection request on
 * that socket, reads in everything until the client closes the
 * connection, and returns the transmitted content.
 *
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class SocketCapturer
{
  Server server;
  Thread thread;
  ServerSocket socket;

  public SocketCapturer()
    throws java.io.IOException
  {
    this(0);
  }


  public SocketCapturer(int port)
    throws java.io.IOException
  {
    socket = new ServerSocket(port);
    server = new Server(socket);
    thread = new Thread(server);
    thread.start();
  }


  public int getLocalPort()
  {
    return socket.getLocalPort();
  }


  /**
   * Returns the transmitted content. Blocks the current thread
   * until a client has transmitted all its content and closed the
   * connection.
   */
  public byte[] getCaptured()
  {
    try
      {
        thread.join();
      }
    catch (InterruptedException _)
      {
      }

    return server.getCaptured();
  }


  private class Server
    implements Runnable
  {
    ServerSocket serverSocket;
    Socket conn;
    private volatile Throwable thrown;
    private volatile byte[] captured;

    public Server(ServerSocket serverSocket)
    {
      this.serverSocket = serverSocket;
    }

    public byte[] getCaptured()
    {
      return captured;
    }

    public void run()
    {
      try
        {
	  conn = serverSocket.accept();
	  InputStream is = conn.getInputStream();
	  ByteArrayOutputStream bos = new ByteArrayOutputStream();
	  int r = 0;
	  byte[] b = new byte[2000];

	  while ((r = is.read(b)) >= 0)
	  {
	    if (r > 0)
	      bos.write(b, 0, r);
	  }

	  conn.close();
	  captured = bos.toByteArray();
	  bos.close();
	}
      catch (Exception ex)
        {
	  this.thrown = ex;
	  ex.printStackTrace();
	}
    }
  }
}
