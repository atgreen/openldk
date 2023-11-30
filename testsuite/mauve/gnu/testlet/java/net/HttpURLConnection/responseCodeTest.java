//Tags: JDK1.1
//Uses: TestHttpServer

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

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.HttpURLConnection;
import java.net.Socket;
import java.net.URL;
import java.util.List;

/**
 * Tests correct behaviour of getInputStream(), getErrorStream()
 * for all error response codes.
 */
public class responseCodeTest implements Testlet
{
  /**
   * Starts an HTTP server and calls
   * the test_ResponseCode for the error codes.
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

        for (int i=400; i < 418; i++)
          test_ResponseCode(i, h, server);
        
        for (int i=500; i < 506; i++)
          test_ResponseCode(i, h, server);
      }
    finally
      {
        if (server != null)
          server.killTestServer();
      }
  }
  
  static class Factory implements TestHttpServer.ConnectionHandlerFactory
  {
    private int responseCode;
    Factory(int responseCode)
    {
      this.responseCode = responseCode;
    }
    
    public TestHttpServer.ConnectionHandler newConnectionHandler(Socket s)
      throws IOException
    {
       return new Handler(s, responseCode);
    }
  }
  
  static class Handler extends TestHttpServer.ConnectionHandler
  {
    private int responseCode;
    private Writer sink;
    
    Handler(Socket socket, int responseCode) throws IOException
    {
      super(socket);
      this.responseCode = responseCode;
      sink = new OutputStreamWriter(output,"US-ASCII");
    }

    protected boolean processConnection(List headers, byte[] body)
      throws IOException
    {
      sink.write("HTTP/1.0 " + responseCode + " OK\r\n");
      sink.write("Server: TestServer\r\n\r\n");
      sink.close();
      return false;
    }
  }
  
  public void test_ResponseCode(int responseCode, 
      TestHarness h, TestHttpServer server)
  {    
    try
      {
        server.setConnectionHandlerFactory(new Factory(responseCode));
        URL url = new URL("http://localhost:" + server.getPort() + "/");
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("GET");
        
        h.checkPoint("Test " + responseCode + " response");

        // test the responsecode
        int code = conn.getResponseCode();
        h.check(code == responseCode);

        // getInputStream should always throw an IOException
        try 
          {
            conn.getInputStream();
            h.check(false);
          }
        catch (IOException e)
          {
            // for a 404/410 it must be a FNFE
            if (responseCode == 404 
                || responseCode == 410)
              {
                // Since JDK 1.5 as FNFE is thrown - so this will fail for 1.4
                if (e instanceof FileNotFoundException)
                  h.check(true);
                else
                  h.check(false);
              }
            else
              h.check(true);
          }
        
        // the errorstream must be set always
        InputStream error = conn.getErrorStream();
        h.check(error != null);
        
        conn.disconnect();       
      }   
    catch (IOException e)
      {       
        h.debug("Unexpected IOException");
        h.debug(e);
      }
  }
}
