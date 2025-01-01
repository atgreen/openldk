//Tags: JDK1.5
//Uses: TestHttpServer

//Copyright (C) 2006 David Daney <ddaney@avtrex.com>

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

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.HttpURLConnection;
import java.net.Socket;
import java.net.URL;
import java.util.List;

/**
 * Tests correct behaviour of keep-alive connections.
 */
public class timeout implements Testlet
{
  /**
   * Starts an HTTP server and runs some tests
   */
  public void test(TestHarness h) 
  {  
    TestHttpServer server = null;
    try
      {
      	try
      	  {
            server = new TestHttpServer();
      	  }
      	catch (IOException ioe)
      	  {
      	    h.debug(ioe);
      	    h.fail("Could not start server");
      	    return;
      	  }
        
        testReadTimeout(h, server);
        server.closeAllConnections();
        testConnectTimeout(h);
      }
    finally
      {
        if (server != null)
          server.killTestServer();
      }
  }

  static class Factory implements TestHttpServer.ConnectionHandlerFactory
  {
    Factory()
    {
    }
    
    public TestHttpServer.ConnectionHandler newConnectionHandler(Socket s)
      throws IOException
    {
      return new Handler(s);
    }
  }

  static class Handler extends TestHttpServer.ConnectionHandler
  {
    private Writer sink;
     
    Handler(Socket socket) throws IOException
    {
      super(socket);
      sink = new OutputStreamWriter(output,"US-ASCII");
    }

    protected boolean processConnection(List headers, byte[] body)
      throws IOException
    {
      boolean closeme = false;
      String request = (String)headers.get(0);
      if (!request.startsWith("GET "))
        {
          sink.write("HTTP/1.1 400 Bad Request\r\n");
          sink.write("Server: TestServer\r\n");
          sink.write("Connection: close\r\n");
          sink.write("\r\n");
          sink.flush();
          return false;
        }
      sink.write("HTTP/1.1 200 OK\r\n");
      sink.write("Server: TestServer\r\n");
      if (request.indexOf("closeme") != -1)
        {
          sink.write("Connection: close\r\n");
          closeme = true;
        }
      sink.write("Content-Length: 7\r\n");
      sink.write("\r\n");
      sink.flush();
      try
        {
    	  Thread.sleep(10000);
        }
      catch (InterruptedException ie)
        {
    	  // Ignore.
        }
      sink.write("Hello\r\n");
      sink.flush();
      return !closeme;
    }
  }

  private static int readFully(InputStream is, byte d[]) throws IOException
  {
    int pos = 0;
    int c;

    while (pos < d.length)
      {
        c = is.read(d, pos, d.length - pos);
        if (c == -1)
          {
            if (pos == 0)
              return -1;
            else
              break;
          }
        pos += c;
      }
    return pos;
  }

  private void testReadTimeout(TestHarness h, TestHttpServer server)
  {    
    try
      {
      	byte data[] = new byte[100];

        h.checkPoint("read-1");

        server.setConnectionHandlerFactory(new Factory());
        // Simple read timeout.
        URL url = new URL("http://127.0.0.1:" + server.getPort() + "/closeme");
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setReadTimeout(5000);
        try
          {
            // test the responsecode        
            int code = conn.getResponseCode();
            InputStream s = conn.getInputStream();
            int v = readFully(s, data);
            // It should time out and never get here.
            h.check(false);
          }
        catch (IOException ioe)
          {
            // It should timeout.
            h.check(true);
          }

        h.checkPoint("read-2");
        // Normal read.  No timeout.
        url = new URL("http://127.0.0.1:" + server.getPort() + "/foo");
        conn = (HttpURLConnection) url.openConnection();

        // test the responsecode
        int code = conn.getResponseCode();
        h.check(code, 200);
 
        InputStream s = conn.getInputStream();
        int v = readFully(s, data);
        s.close();
        h.check(v, 7);

        h.checkPoint("read-3");
        // Set timeout on a reused connection.
        url = new URL("http://127.0.0.1:" + server.getPort() + "/bar");
        conn = (HttpURLConnection) url.openConnection();
        conn.setReadTimeout(5000);
        try
          {
            // test the responsecode        
            code = conn.getResponseCode();
            s = conn.getInputStream();
            v = readFully(s, data);
            // It should time out and never get here.
            h.check(false);
          }
        catch (IOException ioe)
          {
            // It should timeout.
            h.check(true);
          }
      }   
    catch (IOException e)
      {       
        h.debug("Unexpected IOException");
        h.debug(e);
      }
  }

private void testConnectTimeout(TestHarness h)
  {    
    try
      {
      	byte data[] = new byte[100];

        h.checkPoint("connect-1");

        // pick an address that will not be globally routable, but is also
        // not on our local network.  This should generate a connection
        // timeout.
        URL url = new URL("http://10.20.30.40:/foo");
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        long start = System.currentTimeMillis();
        conn.setConnectTimeout(3000);
        try
          {
            // test the responsecode        
            int code = conn.getResponseCode();
            InputStream s = conn.getInputStream();
            int v = readFully(s, data);
            // It should time out and never get here.
            h.check(false);
          }
        catch (IOException ioe)
          {
            // It should timeout.
        	long end = System.currentTimeMillis();
        	long delta = end - start;
            h.check((delta > 0) && (delta < 5000));
          }
      }   
    catch (IOException e)
      {       
        h.debug("Unexpected IOException");
        h.debug(e);
      }
  }
}
