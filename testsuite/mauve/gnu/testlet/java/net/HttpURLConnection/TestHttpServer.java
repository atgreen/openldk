//Tags: not-a-test

//Copyright (C) 2006 Free Software Foundation, Inc.
//Written by Wolfgang Baer (WBaer@gmx.de)

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 51 Franklin Street, Fifth Floor,
//Boston, MA, 02110-1301 USA.

package gnu.testlet.java.net.HttpURLConnection;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;


/**
 * A HTTP server for testing purpose only. The server can
 * be started on a given port and the response headers and
 * response body to be returned set. This way one can test
 * arbritrary testcases for the http client side library.
 *  
 * @see gnu.testlet.java.net.HttpURLConnection.responseCodeTest
 * @see gnu.testlet.java.net.HttpURLConnection.responseHeadersTest
 * 
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class TestHttpServer implements Runnable
{ 
  
  public interface ConnectionHandlerFactory
  {
    ConnectionHandler newConnectionHandler(Socket s) throws IOException;
  }
  
  /**
   * The request handler skeleton.
   */
  public static abstract class ConnectionHandler implements Runnable
  {  
    protected Socket socket;
    protected OutputStream output;
    protected InputStream input;

    ConnectionHandler(Socket socket) throws IOException
    {
      this.socket = socket;
      output = socket.getOutputStream();
      input = socket.getInputStream();
    }

    /**
     * Process one request on the connection.
     * 
     * @param headers
     * @param body
     * @return true if another request should be read from the connection.
     * @throws IOException
     */
    protected abstract boolean processConnection(List headers, byte[] body)
      throws IOException;

    protected String getHeaderFromList(List headers, String h)
    {
      String search = (h + ":").toLowerCase();
      Iterator it = headers.iterator();
      while (it.hasNext())
        {
          String v = (String)it.next();
          String k = v.toLowerCase();
          if (k.startsWith(search))
            return v.substring(search.length()).trim();
        }
      return null;
    }

    public void run()
    {
      try
        {
          List headerList;
          int contentLength = -1;
          byte[] body;
          do
            {
              headerList = new ArrayList();

              ByteArrayOutputStream line;
              line = new ByteArrayOutputStream();
              for (;;)
                {
                  int ch = input.read();
                  if (-1 == ch)
                    break; // EOF
              
                  if (ch !=  0x0a) // LF
                    line.write(ch);
                  else
                    {
                      byte[] array = line.toByteArray();
                      if (array.length == 1) // the last is only a LF
                        break;
        	
                      String headerLine = new String(array);
                      if (headerLine.length() > 15 &&
                          "Content-Length:".equalsIgnoreCase(headerLine.substring(0,15)))
                        {
                          contentLength = Integer.parseInt(headerLine.substring(15).trim());
                        }
                      headerList.add(headerLine);
                      line = new ByteArrayOutputStream();
                    }
                }

              if (contentLength > 0) 
                {
                  body = new byte[contentLength];
                  int pos = 0;
                  while (pos < contentLength)
                    {
                      int nr = input.read(body, pos, body.length - pos);
                      if (-1 == nr)
                        break;
                      pos += nr;
                    }
                }
              else
                body = null;
              contentLength = -1;
              // Check everything
            } while (processConnection(headerList, body));

          // Clean up
          output.close();
          input.close();
          socket.close();            
        }
      catch (Exception e)
        {
          // ignore
        }
    }

    protected void forceClosed()
    {
      try
        {
          socket.close();
        }
      catch (IOException ioe)
        {
          // Ignore.
        }
    }
  }

  boolean kill = false;
  ServerSocket serverSocket;  
  ConnectionHandlerFactory connectionHandlerFactory;
  
  /**
   * Create a TestHttpServer on an unused port.
   */
  public TestHttpServer() throws IOException
  {
    serverSocket = new ServerSocket(0);
    Thread t = new Thread(this, "TestHttpServer");
    t.start();
  }
  
  /**
   * The local port on which the test server is listening for connections.
   * @return the port
   */
  public int getPort()
  {
    return serverSocket.getLocalPort();
  }
  
  public synchronized void setConnectionHandlerFactory(ConnectionHandlerFactory f)
  {
    connectionHandlerFactory = f;
  }
  
  /**
   * This cleans up recources so more than one
   * TestHttpServer can be used in one mauve run.
   */
  public void killTestServer()
  {
    kill = true;
    closeAllConnections();
    try
      {
        serverSocket.close();
      }
    catch (IOException e)
      {
        // ignore
      }
  }
  
  private List activeConnections = new LinkedList();
  
  /**
   * Listens on the port and creates a Handler for
   * incoming connections.
   */
  public void run() 
  {   
    try
      {
        while (! kill)
          {
            Socket socket = serverSocket.accept();
            try
              {
                ConnectionHandlerFactory f;
                synchronized(this)
                  {
                    f = connectionHandlerFactory;
                  }
                ConnectionHandler request = f.newConnectionHandler(socket);
                Thread thread = new Thread(request);
                thread.start();
                synchronized(activeConnections)
                  {
                    activeConnections.add(request);
                  }
              }
            catch (Exception e)
              {
                // ignore
              }
          }
      }
    catch (IOException e)
      {
        // ignore
      }
  }
  
  public void closeAllConnections()
  {
    synchronized (activeConnections)
      {
        Iterator it = activeConnections.iterator();
        while (it.hasNext())
          {
            ConnectionHandler request = (ConnectionHandler)it.next();
            request.forceClosed();
            it.remove();
          }
      }
  }
}
