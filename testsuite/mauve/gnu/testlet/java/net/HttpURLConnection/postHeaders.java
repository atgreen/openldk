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
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.HttpURLConnection;
import java.net.Socket;
import java.net.URL;
import java.util.List;

/**
 * Tests that POST sends correct headers.
 */
public class postHeaders implements Testlet
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

    HandlerP1(Socket socket) throws IOException
    {
      super(socket);
      sink = new OutputStreamWriter(output,"US-ASCII");
    }

    protected boolean processConnection(List headers, byte[] body)
      throws IOException
    {
      String request = (String)headers.get(0);
      String contentType = getHeaderFromList(headers, "content-type");
      if (!request.startsWith("POST ")
	  || contentType == null
	  || !contentType.equals("application/x-www-form-urlencoded"))
        {
          sink.write("HTTP/1.1 400 Bad Request\r\n");
          sink.write("Server: TestServer\r\n");
          sink.write("Connection: close\r\n");
          sink.write("\r\n");
          sink.flush();
          return false;
        }
      sink.write("HTTP/1.1 204 No Content\r\n");
      sink.write("Server: TestServer\r\n");
      sink.write("\r\n");
      sink.flush();
      return true;
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
      }   
    catch (IOException e)
      {       
        h.debug("Unexpected IOException");
        h.debug(e);
      }
  }
}
