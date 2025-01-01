//Tags: JDK1.1
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
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.HttpURLConnection;
import java.net.Socket;
import java.net.URL;
import java.util.List;

/**
 * Tests correct behaviour of keep-alive connections.
 */
public class reuseConnection implements Testlet
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
        
        test_GET(h, server);
        server.closeAllConnections();
        test_HEAD(h, server);
        server.closeAllConnections();
        test_POST(h, server);
        server.closeAllConnections();
      }
    finally
      {
        if (server != null)
          server.killTestServer();
      }
  }

  static class FactoryP1 implements TestHttpServer.ConnectionHandlerFactory
  {
    FactoryP1()
    {
    }
    
    public TestHttpServer.ConnectionHandler newConnectionHandler(Socket s)
      throws IOException
    {
      return new HandlerP1(s);
    }
  }

  static class HandlerP1 extends TestHttpServer.ConnectionHandler
  {
    private Writer sink;
    private int requestNumber;
    
    HandlerP1(Socket socket) throws IOException
    {
      super(socket);
      sink = new OutputStreamWriter(output,"US-ASCII");
    }

    protected boolean processConnection(List headers, byte[] body)
      throws IOException
    {
      requestNumber++;
      String request = (String)headers.get(0);
      if (!request.startsWith("POST "))
        {
          sink.write("HTTP/1.1 400 Bad Request\r\n");
          sink.write("Server: TestServer\r\n");
          sink.write("Connection: close\r\n");
          sink.write("\r\n");
          sink.flush();
          return false;
        }
      sink.write("HTTP/1.1 204 No Content\r\n");

      if (request.indexOf("file1") != -1)
        {
          sink.write("Server: TestServer1\r\n");
        }
      else if (requestNumber == 2)
        {
          sink.write("Server: TestServer2\r\n");
        }
      else
        {
          sink.write("Server: TestServer3\r\n");
        }

      sink.write("\r\n");
      sink.flush();
      return requestNumber < 2;
    }
  }

  static class FactoryG1 implements TestHttpServer.ConnectionHandlerFactory
  {
    FactoryG1()
    {
    }

    public TestHttpServer.ConnectionHandler newConnectionHandler(Socket s)
      throws IOException
    {
      return new HandlerG1(s);
    }
  }

  static class HandlerG1 extends TestHttpServer.ConnectionHandler
  {
    private Writer sink;
    private int requestNumber;

    HandlerG1(Socket socket) throws IOException
    {
      super(socket);
      sink = new OutputStreamWriter(output,"US-ASCII");
    }

    protected boolean processConnection(List headers, byte[] body)
      throws IOException
    {
      boolean hello = false;
      boolean goodBye = false;
      boolean err = false;

      requestNumber++;
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

      if (request.indexOf("file1") != -1)
        {
          sink.write("Content-Length: 5\r\n");
          hello = true;
        }
      else if (requestNumber == 2)
        {
          sink.write("Content-Length: 8\r\n");
          sink.write("Connection: close\r\n");
          goodBye = true;
        }
      else
        {
          sink.write("Content-Length: 3\r\n");
          err = true;
        }
	
      sink.write("\r\n");
      if (hello)
	sink.write("Hello");
      else if (goodBye)
	sink.write("Good Bye");
      else if (err)
	sink.write("Err");

      sink.flush();
      return requestNumber < 2;
    }
  }
  
  static class FactoryH1 implements TestHttpServer.ConnectionHandlerFactory
  {
    FactoryH1()
    {
    }
    
    public TestHttpServer.ConnectionHandler newConnectionHandler(Socket s)
      throws IOException
    {
      return new HandlerH1(s);
    }
  }
  
  static class HandlerH1 extends TestHttpServer.ConnectionHandler
  {
    private Writer sink;
    private int requestNumber;

    HandlerH1(Socket socket) throws IOException
    {
      super(socket);
      sink = new OutputStreamWriter(output,"US-ASCII");
    }

    protected boolean processConnection(List headers, byte[] body)
      throws IOException
    {
      requestNumber++;
      String request = (String)headers.get(0);
      if (!request.startsWith("HEAD "))
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

      if (request.indexOf("file1") != -1)
	sink.write("Content-Length: 100000\r\n");
      else if (requestNumber == 2)
        {
          sink.write("Content-Length: 200000\r\n");
          sink.write("Connection: close\r\n");
        }
      else
	sink.write("Content-Length: 300000\r\n");

      sink.write("\r\n");
      sink.flush();
      return requestNumber < 2;
    }
  }

  static class FactoryH2 implements TestHttpServer.ConnectionHandlerFactory
  {
    FactoryH2()
    {
    }

    public TestHttpServer.ConnectionHandler newConnectionHandler(Socket s)
      throws IOException
    {
      return new HandlerH2(s);
    }
  }

  static class HandlerH2 extends TestHttpServer.ConnectionHandler
  {
    private Writer sink;
    private int requestNumber;

    HandlerH2(Socket socket) throws IOException
    {
      super(socket);
      sink = new OutputStreamWriter(output,"US-ASCII");
    }

    protected boolean processConnection(List headers, byte[] body)
      throws IOException
    {
      requestNumber++;
      String request = (String)headers.get(0);
      if (!request.startsWith("HEAD "))
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
      sink.write("Transfer-Encoding: chunked\r\n");

      if (request.indexOf("fileA") != -1)
	sink.write("Content-Type: text/html\r\n");
      else if (requestNumber == 2)
        {
          sink.write("Content-Type: text/plain\r\n");
          sink.write("Connection: close\r\n");
        }
      else
	sink.write("Content-Type: application/octet-stream\r\n");

      sink.write("\r\n");
      sink.flush();
      return requestNumber < 2;
    }
  }

  public void test_HEAD(TestHarness h, TestHttpServer server)
  {    
    try
      {
        h.checkPoint("HEAD-1");

        server.setConnectionHandlerFactory(new FactoryH1());

        URL url = new URL("http://localhost:" + server.getPort() + "/file1");
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("HEAD");

        // test the responsecode
        int code = conn.getResponseCode();
        h.check(code, 200);
        int contentLength = conn.getContentLength();
        h.check(contentLength, 100000);

        InputStream s = conn.getInputStream();
        int v = s.read();
        h.check(v, -1); // Must be EOF.

        // The errorstream must not be set.
        InputStream error = conn.getErrorStream();
        h.check(error, null);

        h.checkPoint("HEAD-2");

        url = new URL("http://localhost:" + server.getPort() + "/file2");
        conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("HEAD");

        // test the responsecode
        code = conn.getResponseCode();
        h.check(code, 200);
        contentLength = conn.getContentLength();
        h.check(contentLength, 200000);

        s = conn.getInputStream();
        v = s.read();
        h.check(v, -1); // Must be EOF.

        // The errorstream must not be set.
        error = conn.getErrorStream();
        h.check(error, null);
        
        // Now on a new connection reporting chuncked.
        server.setConnectionHandlerFactory(new FactoryH2());

        h.checkPoint("HEAD-3");

        url = new URL("http://localhost:" + server.getPort() + "/fileA");
        conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("HEAD");

        // test the responsecode
        code = conn.getResponseCode();
        h.check(code, 200);
        String contentType = conn.getContentType();
        h.check(contentType, "text/html");

        s = conn.getInputStream();
        v = s.read();
        h.check(v, -1); // Must be EOF.
        
        // The errorstream must not be set.
        error = conn.getErrorStream();
        h.check(error, null);
        
        h.checkPoint("HEAD-4");

        url = new URL("http://localhost:" + server.getPort() + "/fileB");
        conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("HEAD");

        // test the responsecode
        code = conn.getResponseCode();
        h.check(code, 200);
        contentType = conn.getContentType();
        h.check(contentType, "text/plain");

        s = conn.getInputStream();
        v = s.read();
        h.check(v, -1); // Must be EOF.
        
        // The errorstream must not be set.
        error = conn.getErrorStream();
        h.check(error, null);
        
      }   
    catch (IOException e)
      {       
        h.debug("Unexpected IOException");
        h.debug(e);
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

  public void test_GET(TestHarness h, TestHttpServer server)
  {    
    try
      {
      	byte data[] = new byte[100];

        h.checkPoint("GET-1");

        server.setConnectionHandlerFactory(new FactoryG1());

        URL url = new URL("http://localhost:" + server.getPort() + "/file1");
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();

        // test the responsecode
        int code = conn.getResponseCode();
        h.check(code, 200);
        int contentLength = conn.getContentLength();
        h.check(contentLength, 5);
  
        InputStream s = conn.getInputStream();
        int v = readFully(s, data);
        h.check(v, 5);

        // The errorstream must not be set.
        InputStream error = conn.getErrorStream();
        h.check(error, null);
  
        h.checkPoint("GET-2");
        
        url = new URL("http://localhost:" + server.getPort() + "/file2");
        conn = (HttpURLConnection) url.openConnection();

        // test the responsecode
        code = conn.getResponseCode();
        h.check(code, 200);
        contentLength = conn.getContentLength();
        h.check(contentLength, 8);
  
        s = conn.getInputStream();
        v = readFully(s, data);
        h.check(v, 8);

        // The errorstream must not be set.
        error = conn.getErrorStream();
        h.check(error, null);
      }   
    catch (IOException e)
      {       
        h.debug("Unexpected IOException");
        h.debug(e);
      }
  }

  public void test_POST(TestHarness h, TestHttpServer server)
  {    
    try
      {
      	byte data[] = new byte[] {'M', 'e', 's', 's', 'a', 'g', 'e'};

        h.checkPoint("POST-1");
        server.setConnectionHandlerFactory(new FactoryP1());

        URL url = new URL("http://localhost:" + server.getPort() + "/file1");
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("POST");
        conn.setDoOutput(true);
        OutputStream os = conn.getOutputStream();
        os.write(data);
        os.close();

        // test the responsecode
        int code = conn.getResponseCode();
        h.check(code, 204);
        String serverName = conn.getHeaderField("Server");
        h.check(serverName, "TestServer1");

        // The errorstream must not be set.
        InputStream error = conn.getErrorStream();
        h.check(error, null);

        h.checkPoint("POST-2");
        url = new URL("http://localhost:" + server.getPort() + "/file2");
        conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("POST");
        conn.setDoOutput(true);
        os = conn.getOutputStream();
        os.write(data);
        os.close();

        // test the responsecode
        code = conn.getResponseCode();
        h.check(code, 204);
        serverName = conn.getHeaderField("Server");
        h.check(serverName, "TestServer2");

        // The errorstream must not be set.
        error = conn.getErrorStream();
        h.check(error, null);
      }   
    catch (IOException e)
      {       
        h.debug("Unexpected IOException");
        h.debug(e);
      }
  }
}
