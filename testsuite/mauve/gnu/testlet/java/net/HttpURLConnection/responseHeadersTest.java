//Tags: JDK1.4
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

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.HttpURLConnection;
import java.net.Socket;
import java.net.URL;
import java.util.List;
import java.util.Map;

public class responseHeadersTest implements Testlet
{
  public void test(TestHarness harness)
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
    	    harness.debug(ioe);
    	    harness.fail("Could not start server");
    	    return;
    	  }
        test_MultiHeaders(harness, server);
        test_LowerUpperCaseHeaders(harness, server);
      }
    finally
      {
        server.killTestServer();
      }
  }

  static class Factory implements TestHttpServer.ConnectionHandlerFactory
  {
    private String headers;
    Factory(String headers)
    {
      this.headers = headers;
    }

    public TestHttpServer.ConnectionHandler newConnectionHandler(Socket s)
      throws IOException
    {
       return new Handler(s, headers);
    }
  }

  static class Handler extends TestHttpServer.ConnectionHandler
  {
    private String responseHeaders;
    private Writer sink;

    Handler(Socket socket, String headers) throws IOException
    {
      super(socket);
      this.responseHeaders = headers;
      sink = new OutputStreamWriter(output,"US-ASCII");
    }

    protected boolean processConnection(List headers, byte[] body)
      throws IOException
    {
      sink.write(responseHeaders);
      sink.close();
      return false;
    }
  }

  public void test_MultiHeaders(TestHarness h, TestHttpServer server)
  {    
    try
      {
        Factory f = new Factory(
           "HTTP/1.0 200 OK\r\n" +
           "Server: TestServer\r\n" +
           "Key1: value, value2\r\n" +
           // set the header a second time with different values
           // these values must be prepended to key1
           "Key1: value3\r\n" +
           "IntHeader: 1234\r\n" +
           "IntHeaderMalformed: 1234XY\r\n" +
           "DateHeader: Thu, 02 Mar 2006 14:34:55 +0000\r\n" +
           "DateHeaderMalformed: Thu, 02 Mar 2006V 14:13:07 +0000\r\n\r\n"
           );

        server.setConnectionHandlerFactory(f);

        URL url = new URL("http://localhost:" + server.getPort() + "/");
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("GET");

        h.checkPoint("getHeaderFields()");
        Map fields = conn.getHeaderFields();
        
        // check that map is unmodifiable
        try 
          {
            fields.clear();
            h.check(false);
          }
        catch (UnsupportedOperationException e) 
          {
            h.check(true);
          }
      
        // exactly 7 headers with status and server header        
        h.check(fields.size() == 7);
        
        // check for list and that case matters for key
        Object obj = fields.get("Key1");
        if (! (obj instanceof List))
          h.check(false);
        else
          {
            h.check(true);
            List value = (List) obj;
            h.check(value.size() == 2);
            h.check(value.get(0).equals("value3"));
            h.check(value.get(1).equals("value, value2"));
            
            // check that it is an unmodifiable list
            try 
              {
                value.remove(0);
                h.check(false);
              }
            catch (UnsupportedOperationException e) 
              {
                h.check(true);
              }
          }
        
        // wrong case for key
        obj = fields.get("key1");
        h.check(obj == null);
          
        
        // checks for getHeaderField/Key(int)        
        h.checkPoint("getHeaderField(int)");
        // check that index 0 is the statusline
        String statusline = conn.getHeaderField(0);
        h.check(statusline.equals("HTTP/1.0 200 OK"));
        // indexes out of bound must return null
        String aboutIndex = conn.getHeaderField(44);
        h.check(aboutIndex == null);
        String belowIndex = conn.getHeaderField(-1);
        h.check(belowIndex == null);
        // check that correct key/value name is returned
        String key1_Value = conn.getHeaderField(2);
        h.check(key1_Value.equals("value, value2"));
        
        h.checkPoint("getHeaderFieldKey(int)");       
        // check that index 0 is the statusline
        String statuslineKey = conn.getHeaderFieldKey(0);        
        h.check(statuslineKey == null);
        // indexes out of bound must return null
        String aboutIndexKey = conn.getHeaderFieldKey(44);        
        h.check(aboutIndexKey == null);
        String belowIndexKey = conn.getHeaderFieldKey(-1);
        h.check(belowIndexKey == null);
        // check that correct key/value name is returned
        String key1_Key = conn.getHeaderFieldKey(2);
        h.check(key1_Key.equals("Key1"));
        
        
        // checks getHeaderFieldDate
        h.checkPoint("getHeaderFieldDate()");
        // correct date header field
        long dateHeader = conn.getHeaderFieldDate("DateHeader", 5555);
        h.check(dateHeader == 1141310095000L);
        // missing date header field
        dateHeader = conn.getHeaderFieldDate("DateHeaderXX", 5555);
        h.check(dateHeader == 5555);
        // malformed date header value 
        dateHeader = conn.getHeaderFieldDate("DateHeaderMalformed", 5555);
        h.check(dateHeader == 5555);
        
        // checks getHeaderFieldInt
        h.checkPoint("getHeaderFieldInt()");
        // correct int header field
        int intHeader = conn.getHeaderFieldInt("IntHeader", 5555);
        h.check(intHeader == 1234);
        // missing int header field
        intHeader = conn.getHeaderFieldInt("IntHeaderXX", 5555);
        h.check(intHeader == 5555);
        // malformed int header value 
        intHeader = conn.getHeaderFieldInt("IntHeaderMalformed", 5555);
        h.check(intHeader == 5555);
        
        
        // checks that the convenience methods of the headers
        // not set in this test return the correct default values
        h.checkPoint("convenience methods");
        h.check(conn.getLastModified() == 0);
        h.check(conn.getDate() == 0);
        h.check(conn.getExpiration() == 0);
        h.check(conn.getContentEncoding() == null);
        h.check(conn.getContentType() == null);
        h.check(conn.getContentLength() == -1);
        
        
        // checks getHeaderField(String)
        h.checkPoint("getHeaderField(String)");
        String field = conn.getHeaderField("Server");
        String field1 = conn.getHeaderField("server");
        h.check(field.equals("TestServer"));
        h.check(field == field1);
        
        String none = conn.getHeaderField("NotExistent");
        h.check(none == null);
        
        // check for multiple times same key
        String field_key1 = conn.getHeaderField("Key1");
        h.check(field_key1.equals("value3"));

      }
    catch (IOException e)
    {
      h.debug("Unexpected IOException");
      h.debug(e);
    }
    catch (RuntimeException e)
      {
        h.debug("Unexpected IOException");
        h.debug(e);
      }
    
  }
  
  public void test_LowerUpperCaseHeaders(TestHarness h, TestHttpServer server)
  {    
    try
      {
        Factory f = new Factory(
           "HTTP/1.0 200 OK\r\n" +
           "Server: TestServer\r\n" +
           "AnotherKey: value\r\n" +
           "Key: value\r\n" +
           "Key: value1\r\n" +
           "key: value2\r\n" +
           "key: value3\r\n\r\n"
        );
        server.setConnectionHandlerFactory(f);
      
        URL url = new URL("http://localhost:" + server.getPort() + "/");        
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("GET");

        h.checkPoint("LowerUpperCase header fields tests");
        
        Map fields = conn.getHeaderFields();  
                     
        // exactly 5 headers with status and server header        
        h.check(fields.size() == 5);
        
        // check for list and that case matters for key
        List value = (List) fields.get("Key");
        h.check(value.size() == 2);
        h.check(value.get(0).equals("value1"));
        
        List value2 = (List) fields.get("key");
        h.check(value2.size() == 2);
        h.check(value2.get(0).equals("value3"));
        
        List value3 = (List) fields.get("AnotherKey");
        h.check(value3.get(0).equals("value"));
        
        value3 = (List) fields.get("anotherkey");
        h.check(value3 == null);
                     
        // checks getHeaderField(String)
        String field = conn.getHeaderField("Key");
        String field1 = conn.getHeaderField("key");
        h.check(field.equals("value3"));
        h.check(field == field1);
      }
    catch (IOException e)
    {
      h.debug("Unexpected IOException");
      h.debug(e);
    }
    catch (RuntimeException e)
      {
        h.debug("Unexpected IOException");
        h.debug(e);
      }
  }
}
