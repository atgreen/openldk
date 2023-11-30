// Tags: JDK1.0
// Uses: MyHttpURLConnection MyURLConnection

/*
   Copyright (C) 1999 Hewlett-Packard Company

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

package gnu.testlet.java.net.URLConnection;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.net.*;
import java.io.IOException; 


public class URLConnectionTest implements Testlet
{
  protected static TestHarness harness;
  public void test_Basics()
  {
    harness.checkPoint("Basics");
    try {
      URL _url = new URL("http", new String(), "index.html");
      try
	{
	  _url.openConnection();
	  harness.check(true);
	}
      catch(IOException e)
	{
	  harness.fail("Error: Handler - 55");
	}
			
      URL url = new URL("http://sources.redhat.com:80/mauve/testarea/index.html" );

      URLConnection conn = url.openConnection();

      harness.check (!(((HttpURLConnection)conn).usingProxy()), "Error: test_Basics - 50");

      ((HttpURLConnection)conn).disconnect();
      ((HttpURLConnection)conn).setRequestProperty("c", "d");
      String _tmp = ((HttpURLConnection)conn).getRequestProperty("c");
      harness.check (_tmp, "d", "Error: test_Basics - 51");

      ((HttpURLConnection)conn).disconnect();

      harness.check ( conn.getURL(), url, "Error in test_Basics  - 1 " + 
			   " getURL did not return the same URL ");
    }
    catch ( MalformedURLException e )
      {
	harness.fail("Error in test_Basics  - 2 " + 
			   " should not have raised malformed URL exception here " );
      }
    catch ( IOException e )
      {
	harness.fail("Error in test_Basics  - 2 " + 
			   " should not have raised IO exception here " );
      }
    catch ( Exception e )
      {
	harness.debug(e);
	harness.fail("Error in test_Basics  - 2 " + 
			   " should not have raised  exception here " );
      }
    catch ( Throwable e )
      {
	harness.debug(e);
	harness.fail("Error in test_Basics  - 2 " + 
			   " should not have raised  Throwable here " );
      }
		
  }

  public void test_allowUserInteractions()
  {
    harness.checkPoint("allowUserInteractions");
    try {
      URLConnection.setDefaultAllowUserInteraction( false );

      URL url = new URL ( "http://sources.redhat.com/mauve/testarea/index.html");

      URLConnection conn = url.openConnection();

      harness.check ( !(URLConnection.getDefaultAllowUserInteraction()),
		"Error in test_allowUserInteractions  - 1 " + 
		" getDefaultAllowUserInteraction returned wrong values " );

      boolean bool = conn.getAllowUserInteraction();

      harness.check ( ! bool, "Error in test_allowUserInteractions  - 2 " + 
			   " getAllowUserInteraction returned wrong values " );

    }
    catch ( Exception e )
      {
	harness.fail("Error in test_allowUserInteractions  - 3 " + 
			   " should not have raised  exception here " );
      }
  }


  public void test_getContentFunctions()
  {
    harness.checkPoint("getContentFunctions");
    try {
      URL url = new URL ( "http://sources.redhat.com/mauve/testarea/index.html");

      URLConnection conn = url.openConnection();

      // Cannot actually check size since it may return -1
      int siz = conn.getContentLength();

      String type = conn.getContentType();
      String enc = conn.getContentEncoding();

      long dt = conn.getDate();

      java.io.InputStream is = (java.io.InputStream)conn.getContent();

      byte b[] = new byte[256];
      is.read( b , 0 , b.length);

      String cont = new String( b );

      harness.check( type.indexOf("text/html") != -1,
			   "Error in test_getContentFunctions  - 1 " + 
			   " content type was not correct " );

      harness.check( enc, null, "Error in test_getContentFunctions  - 2 " + 
			   "encoding  was not correct " );

      harness.check ( cont.indexOf("<!DOCTYPE HTML PUBLIC") != -1,
			   "Error in test_getContentFunctions  - 4 " + 
			   "getContent did not return proper results "  );
    }
    catch ( Exception e )
      {
	harness.debug(e);
	harness.fail("Error in test_getContentFunctions  - 5 " + 
			   " should not have raised  exception here " );
      }

  }

  public void test_streams()
  {
    harness.checkPoint("streams");
    try {
      URL url = new URL ( "http://sources.redhat.com/mauve/testarea/index.html");

      URLConnection conn = url.openConnection();

      java.io.InputStream  is = conn.getInputStream();


      byte b[] = new byte[17];
      is.read( b , 0 , b.length);
      is.read( b , 0 , b.length);
      harness.check(true);
    }
    catch ( Exception e )
      {
	harness.fail("Error in test_streams  - 1 " + 
			   " should not have raised  exception here " );
      }

    try {
      URL url = new URL ( "http://sources.redhat.com/mauve/testarea/index.html");

      URLConnection conn = url.openConnection();

      java.io.OutputStream  os = conn.getOutputStream();

      byte b[] = new byte[17];
      os.write( b , 0 , b.length);
      harness.fail("Error in test_streams  - 2 " + 
			 " should have raised  protocol exception here " );
			
    }
    catch ( Exception e )
      {
	harness.check(true);
      }
  }


  public void test_DefaultRequestProperty()
  {
    harness.checkPoint("DefaultRequestProperty");
    try {
      URL url = new URL ( "http://sources.redhat.com/mauve/testarea/index.html");
	
      String str = HttpURLConnection.getDefaultRequestProperty("ACCEPT");

      URLConnection.setDefaultRequestProperty( "ACCEPT" , "Ok accept" );
      URLConnection conn = url.openConnection();
			
      str = URLConnection.getDefaultRequestProperty("ACCEPT");

      str = null;
      harness.check(true);
    }
    catch ( Exception e )
      {
	harness.fail("Error in test_DefaultRequestProperty  - 1 " + 
			   " should not have raised exception here " );
      }
  }


  public void test_DefaultUseCaches()
  {
    harness.checkPoint("DefaultUseCaches");
    try {
      URL url = new URL ( "http://sources.redhat.com/mauve/testarea/index.html");
      URLConnection conn = url.openConnection();

      boolean bool = conn.getDefaultUseCaches();
      //if ( bool )
      //	harness.fail("Error in test_DefaultUseCaches - 1 " + 
      //		" getDefaultUseCaches failed " );

      conn.setDefaultUseCaches(true);
      bool = conn.getDefaultUseCaches();
      harness.check ( bool, "Error in test_DefaultUseCaches - 2 " + 
			   " get/setDefaultUseCaches failed " );

      String str = null;
    }
    catch ( Exception e )
      {
	harness.fail("Error in test_DefaultRequestProperty  - 3 " + 
			   " should not have raised exception here " );
      }
  }


  public void test_DoInputOutput()
  {
    harness.checkPoint("DoInputOutput");
    try {
      URL url = new URL ( "http://sources.redhat.com/mauve/testarea/index.html");
      URLConnection conn = url.openConnection();

      boolean bool = conn.getDoInput();
      //if ( bool )
      //	harness.fail("Error in test_DefaultUseCaches - 1 " + 
      //		" getDefaultUseCaches failed " );

      conn.setDoInput( true );
      bool = conn.getDoInput();
      harness.check ( bool, "Error in test_DoInputOutput - 1 " + 
			   " get/setDoInput failed " );

      conn.setDoOutput( true );
      bool = conn.getDoOutput();
      harness.check ( bool, "Error in test_DoInputOutput - 2 " + 
			   " get/setdooutput failed " );
    }
    catch ( Exception e )
      {
	harness.fail("Error in test_DoInputOutput  - 3 " + 
			   " should not have raised exception here " );
      }
  }

  public void test_getHeaderField()
  {
    harness.checkPoint("getHeaderField");
    try {
      URL url = new URL ( "http://sources.redhat.com/mauve/testarea/index.html");
      URLConnection conn = url.openConnection();

      String str0 = conn.getHeaderField(0);
      harness.debug("header-0: " + str0);
      String str1 = conn.getHeaderField("Date");
      harness.debug("header-1: " + str1);
      String str2 = conn.getHeaderField("Server");
      harness.debug("header-2: " + str2);
      String str3 = conn.getHeaderField("Last-Modified");
      harness.debug("header-3: " + str3);    
      String str5 = conn.getHeaderField("Content-Type");
      harness.debug("header-5: " + str5);

      harness.check ( str0, "HTTP/1.1 200 OK",
			   "Error in test_getHeaderField  - 0 " + 
			   " 0 header field wrong" );

      harness.check ( str2.indexOf("Apache") != -1,
			   "Error in test_getHeaderField  - 1 " + 
			   " 2 header field wrong" );

      harness.check ( str5.indexOf("text/html") != -1,
			   "Error in test_getHeaderField  - 3 " + 
			   " 5 header field wrong" );


      ((HttpURLConnection)conn).disconnect();
    }
    catch ( Exception e )
      {
	harness.fail("Error in test_getHeaderField  - 10 " + 
			   " should not have raised exception here " );
      }
  }

  public void test_URLConnection()
  {
    harness.checkPoint("URLConnection");
    try {
      URL url = new URL ( "http://sources.redhat.com/mauve/testarea/index.html");
      MyURLConnection conn = new MyURLConnection(url);

      harness.check(conn.getURL(), url,
      	"Error in test_URLConnection - 1 " + conn.getURL());
      harness.check(conn.getContentLength(), -1,
	"Error in test_URLConnection - 2 " + conn.getContentLength());
      conn.getContentType();
      conn.getContentEncoding();
      long _tmp5 = conn.getExpiration();
      harness.check(_tmp5, 0,
	"Error in test_URLConnection - 5 " + conn.getExpiration());
      harness.check(conn.getDate(), 0,
	"Error in test_URLConnection - 6 " + conn.getDate());
      harness.check(conn.getLastModified(), 0,
	"Error in test_URLConnection - 7 " + conn.getLastModified());
      conn.getHeaderField(0);
      int _tmp = conn.getHeaderFieldInt("", 0);
      harness.check(_tmp, 0,
	"Error in test_URLConnection - 9 ");
      long _tmp2 = conn.getHeaderFieldDate("", 0);
      harness.check(_tmp2, 0,
	"Error in test_URLConnection - 10 ");
      harness.check(conn.getHeaderFieldKey(0), null,
	"Error in test_URLConnection - 11 ");
      harness.check(conn.getHeaderField(null), null,
	"Error in test_URLConnection - 12 ");
      harness.check(conn.getHeaderField(0), null,
	"Error in test_URLConnection - 12a ");
      try {
	conn.getContent();
	harness.fail("Error in test_URLConnection - 12aa");
      }
      catch (UnknownServiceException e)
	{
	  harness.check(true);
	}

      try {
	conn.getInputStream();
	harness.fail("Error in test_URLConnection - getInputStream");
      }
      catch (UnknownServiceException e)
	{
	  harness.check(true);
	}

      try {
	conn.getOutputStream();
	harness.fail("Error in test_URLConnection - getOutputStream");
      }
      catch (UnknownServiceException e)
	{
	  harness.check(true);
	}

      harness.check(conn.toString().indexOf(url.toString()) != -1,
	"Error in test_URLConnection - 12b ");
      conn.setDoInput(true);
      harness.check(conn.getDoInput(), "Error in test_URLConnection - 13 ");
      conn.setDoOutput(true);
      harness.check(conn.getDoOutput(), "Error in test_URLConnection - 14 ");
      conn.setAllowUserInteraction(true);
      harness.check(conn.getAllowUserInteraction(),
	"Error in test_URLConnection - 15 ");
      URLConnection.setDefaultAllowUserInteraction(true);
      harness.check(URLConnection.getDefaultAllowUserInteraction(),
	"Error in test_URLConnection - 16 ");
      conn.setUseCaches(true);
      harness.check(conn.getUseCaches(), "Error in test_URLConnection - 17 ");
      conn.setIfModifiedSince(45);
      harness.check(conn.getIfModifiedSince(), 45,
	"Error in test_URLConnection - 18 ");
      conn.setDefaultUseCaches(true);
      harness.check(conn.getDefaultUseCaches(),
	"Error in test_URLConnection - 19 ");
      conn.setRequestProperty("a", "b");
      conn.getRequestProperty("a");
      MyURLConnection.setDefaultRequestProperty("c","d");
      MyURLConnection.getDefaultRequestProperty("c");
      MyURLConnection.setContentHandlerFactory(null);
      //	    if(!MyURLConnection.guessContentTypeFromName("a").equals(""))
      //	      harness.fail("Error in test_URLConnection - 22 ");
      //	    if(!MyURLConnection.guessContentTypeFromStream(null).equals(""))
      //	      harness.fail("Error in test_URLConnection - 22 ");
    } catch (Exception e) {
        harness.debug(e);
	harness.fail("Error in test_URLConnection  - 23 " + 
			   " should not have raised  exception here " );
    }
  }

  public void test_HttpURLConnection()
  {
    harness.checkPoint("HttpURLConnection");
    try {
      URL url = new URL ( "http://sources.redhat.com/mauve/testarea/index.html");
      MyHttpURLConnection conn = new MyHttpURLConnection(url);
      conn.setRequestMethod("GET");
      harness.check(conn.getRequestMethod(), "GET",
	"Error in test_HttpURLConnection - 1 ");
      conn.getResponseCode();
      conn.getResponseMessage();
      MyHttpURLConnection.setFollowRedirects(true);
      harness.check(MyHttpURLConnection.getFollowRedirects(),
	"Error in test_HttpURLConnection - 2 ");
    } catch (Exception e) {
      harness.debug(e);
      harness.fail("Error in test_HttpURLConnection - 3 " +
			   " should not have raised  exception here " );
    }
  }

  public void test_HttpURLConnectionI()
  {
    harness.checkPoint("HttpURLConnectionI");
    try {
      URL url = new URL ( "http://sources.redhat.com/mauve/testarea/index.html");
      HttpURLConnection conn = (HttpURLConnection) url.openConnection();
      conn.getExpiration();
      conn.getLastModified();
      conn.getHeaderField("Host");
      conn.getHeaderField(1);
      conn.usingProxy();
      harness.check(true);
    } catch (Exception e) {
      harness.debug(e);
      harness.fail("Error in test_HttpURLConnectionI - 1 " +
			   " should not have raised  exception here " );
    }
  }

  public void testall()
  {
    test_Basics();
    test_allowUserInteractions();
    test_getContentFunctions();
    test_DefaultRequestProperty();
    test_DefaultUseCaches();
    test_DoInputOutput();
    test_getHeaderField();
    test_streams();
    test_URLConnection();
    test_HttpURLConnection();
    test_HttpURLConnectionI();
  }

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }

}
