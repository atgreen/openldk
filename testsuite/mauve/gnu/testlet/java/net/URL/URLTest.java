// Tags: JDK1.0
// Uses: MyURLStreamHandler

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

package gnu.testlet.java.net.URL;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.io.IOException;
import java.net.*;


public class URLTest implements Testlet
{
  protected static TestHarness harness;
	public void test_Basics()
	{
		boolean ok;
		
		// see whether malformed exception is thrown or not.

		harness.checkPoint("Constructors");

		ok = false;
		try {
			URL url = new URL("hithleksjf" );
		}
		catch ( MalformedURLException e ){
			ok = true;
			harness.check(true);
		}
		harness.check(ok, "Error in test_Basics - 1 " + 
			" should have raised malformed URL exception here");

		ok = false;
		try {
			URL url = new URL("http://////://" );
			ok = true;
		}
		catch ( MalformedURLException e ){
		}
		harness.check(ok, "Error in test_Basics  - 2 " + 
			" should not have raised malformed URL exception here");


		ok = false;
		try {
			URL url = new URL("http://sources.redhat.com/index.html" );
			ok = true;
		}
		catch ( MalformedURLException e ){
		}
		harness.check(ok, "Error in test_Basics  - 3 " +
			" should not have raised malformed URL exception here");

		ok = false;
		try {
			URL url = new URL((String) null);
		}
		catch (MalformedURLException e) {
			ok = true;
		}
		harness.check(ok, "Error in test_Basics  - 4 " +
			" should have raised malformed URL exception here");

		// URL with individual arguments.
		harness.checkPoint("get Methods");
		try {
			URL baseurl = new URL("http://sources.redhat.com/");
			URL url = new URL ( baseurl, "index.html");
			url.hashCode();
			baseurl.hashCode();
			URL.setURLStreamHandlerFactory( null );
			URL.setURLStreamHandlerFactory( null );
			harness.check (url.getProtocol(), "http");
			harness.check (url.getPort(), -1);
			harness.check (url.getHost(), "sources.redhat.com");
			harness.check (url.getFile(), "/index.html");
			harness.check (url.equals(new URL("http://sources.redhat.com/index.html")));
			harness.check (url.hashCode() != 0);
		}
		catch ( MalformedURLException e ){
				harness.fail(" Error in test_Basics  - 9 " + 
					" exception should not be thrown here");
		}


		try {
			URL url = new URL ( "http", "sources.redhat.com", "/index.html");

			harness.check (url.getProtocol(), "http");
			harness.check (url.getPort(), -1);
			harness.check (url.getHost(), "sources.redhat.com");
			harness.check (url.getFile(), "/index.html");
			harness.check (url.equals(new URL("http://sources.redhat.com/index.html")));

			URL url1 = new URL ( "http", "sources.redhat.com", 80,  "/index.html");
			harness.check (url1.getPort(), 80);
			harness.check(url.equals(url1));
			harness.check(url1.toExternalForm(),
					"http://sources.redhat.com:80/index.html");
		}
		catch ( MalformedURLException e ){
				harness.fail(" Error in test_Basics  - 16 " + 
					" exception should not be thrown here");
		}


		try {
			URL url = new URL ( "http://sources.redhat.com:80/mauve/testarea/index.html");

			harness.check (url.getProtocol(), "http");
			harness.check (url.getPort(), 80);
			harness.check (url.getHost(), "sources.redhat.com");
			harness.check (url.getFile(), "/mauve/testarea/index.html");
		}
		catch ( MalformedURLException e ){
				harness.fail(" Error in test_Basics  - 21 " + 
					" exception should not be thrown here");
		}

                try {
                        URL u1 = new URL("http://foo@some.nice.place/bar/");
                        URL u2 = new URL("http://some.nice.place/bar/");
                        URL u3 = new URL(u1, "more/path", null);

                        harness.check (u1.getUserInfo(), "foo");
                        harness.check (u2.getUserInfo(), null);
                        harness.check (u3.getUserInfo(), "foo");
                        harness.check (u3.getProtocol(), "http");
                        harness.check (u3.getHost(), "some.nice.place");
                }
                catch ( MalformedURLException e ){
				harness.fail(" Error in test_Basics  - 27 " + 
                                             " exception should not be thrown here");
                }
                
		try {
			URL u1 = new URL("http://domain.com");
			URL u2 = new URL(u1, "/redir?http://domain2.com/index.html");

			harness.check (u2.getProtocol(), "http");
			harness.check (u2.getHost(), "domain.com");
			harness.check (u2.getPath(), "/redir");
			harness.check (u2.getQuery(), "http://domain2.com/index.html");
                }
                catch ( MalformedURLException e ){
				harness.fail(" Error in test_Basics  - 35 " + 
                                             " exception should not be thrown here");
                }

		harness.checkPoint("Null context handler");
		try
		{
			URL u = new URL(null, "http://sources.redhat.com/");

			harness.check(true);
		}
                catch ( MalformedURLException e ){
			harness.fail(" Error in test_Basics - null context");
		}
		catch (NullPointerException e) {
			harness.fail(" Error in test_Basics - null context");
		}
                harness.checkPoint("Colon in spec");
                try {
                    URL cxt = new URL("http://www.foo.bar.com");
                    URL url = new URL(cxt, "_urn:testing/");
                    harness.check("http://www.foo.bar.com/_urn:testing/".equals(url.toString()));
                }
                catch (Exception e) {
                    harness.fail(" Error in test_Basics - Colon in spec");
                }
	}

	public void test_openConnection()
	{
		harness.checkPoint("openConnection");
		try {
			URL url = new URL ( "http://sources.redhat.com/mauve/testarea/index.html");

			URLConnection conn = url.openConnection();

			if (conn == null) {
				harness.fail("openConnection returned null");
				return;
			}

			String headerField = conn.getHeaderField(2);
			harness.check (headerField != null
					&& headerField.indexOf("Apache") != -1,
							"I want my Apache server!");
			String conttype	= conn.getContentType();
			harness.check (conttype != null
					&& conttype.indexOf("text/html") != -1,
							"Content must be text/html");

			try {
				Object obj = url.getContent();
			} catch (Throwable t) {
				harness.fail("getContent() threw Exception");
				harness.debug(t);
			}
			harness.check (url.toExternalForm(),
				"http://sources.redhat.com/mauve/testarea/index.html");
			harness.check (url.getRef(), null);

			URL url2 = new URL("http://www.hhp.com/index.html#help");
			harness.check (url2.getRef(), "help");
		}catch ( Exception e ){
				harness.fail(" Error in test_openConnection  - 3 " + 
					" exception should not be thrown here");
				harness.debug(e);
		}		

	}



	public void test_openStream()
	{
		harness.checkPoint("openStream");
		try {
			harness.debug("creating URL");
			URL url = new URL ( "http://sources.redhat.com/mauve/testarea/index.html");
			harness.debug("opening stream");
			java.io.InputStream conn = url.openStream();

			byte b [] = new byte[256];
			harness.debug("reding from stream");
			conn.read(b , 0 , 256 );

			String str = new String( b ) ;
			harness.check (str.indexOf("HTML") != -1,
							"Need some HTML");

		}catch ( Exception e ){
			harness.fail(" Error in test_openStream  - 2 " + 
					" exception should not be thrown here");
			harness.debug(e);
		}		

	}


	public void test_sameFile()
	{
		harness.checkPoint("sameFile");
		try {
			URL url = new URL ( "http://sources.redhat.com/mauve/testarea/index.html");
			URL url1 = new URL ( "http://sources.redhat.com/mauve/testarea/index.html");
			harness.check (url.sameFile(url1));

			URL url2 = new URL ( "http://sources.redhat.com:80/mauve/testarea/index.html");
			harness.check (url.sameFile(url2));

		}catch ( Exception e ){
			harness.fail(" Error in test_sameFile  - 3 " + 
					" exception should not be thrown here");
		}

	}
	

	public void test_toString()
	{
		harness.checkPoint("toString");
		try {
			URL url = new URL ( "http://sources.redhat.com/index.html");
			String str = url.toString();

			URL url1 = new URL ( "http://sources.redhat.com:80/mauve/testarea/index.html");
			String str1 = url1.toString();

			URL url2 = new URL ( "http://205.180.83.71/");
			String str2 = url2.toString();

			harness.check (str, "http://sources.redhat.com/index.html");
			harness.check (str1, "http://sources.redhat.com:80/mauve/testarea/index.html");
			harness.check (str2, "http://205.180.83.71/");

			URL url3 = new URL( "ftp" , "sources.redhat.com" , 21 , "/dir/dir1.lst");
			String str3 = url3.toString( );

			harness.check (str3, "ftp://sources.redhat.com:21/dir/dir1.lst");
		}catch ( Exception e ){
			harness.debug(e);
			harness.fail(" Error in test_toString  - 5 " + 
					" exception should not be thrown here");
		}		
	}

	public void test_URLStreamHandler()
	{
		harness.checkPoint("URLStreamHandler");
		try {
		  URL url = new URL ( "http://sources.redhat.com/index.html");
		// test URLStreamHandler
 		MyURLStreamHandler sh = new MyURLStreamHandler();
 		sh.invoke_setURL(url, "http", "sources.redhat.com", 80, "/index.html", "#ref");
		harness.check(true);
 		sh.invoke_parseURL(url, "http://sources.redhat.com/index.html", 0, 20);
		harness.check(true);
		}catch ( MalformedURLException e ){
			harness.fail(" Error in test_URLStreamHandler  - 1 " + 
					" exception should not be thrown here");
		}
		
		harness.checkPoint("inherit URLStreamHandler");
		try {
		    URL base = new URL("acme",
				       "www.redhat.com",
				       80,
				       "/docs/",
				       new MyURLStreamHandler());
		    URL other = new URL(base, "manuals/enterprise/");
		    harness.check(other.toString(),
		    		"acme://www.redhat.com:80/docs/manuals/enterprise/");
		} catch (IOException _) {
			harness.check(false);
			harness.debug(_);
		}

		harness.checkPoint("jar base with full http spec");
		try {
		    URL base = new URL("jar:file:///test.jar!/foo/bar.txt");
		    URL other = new URL(base, "http://planet.classpath.org/");
		    harness.check(other.toString(),
		    		"http://planet.classpath.org/");
		} catch (IOException _) {
			harness.check(false);
			harness.debug(_);
		}
	}


        public void test_cr601a() {
            String[][] s = {

                // tests 0..3
                {"file:////c:/pub/files/foobar.txt",
                 "file:////c:/pub/files/foobar.txt",
                 "",
                 "//c:/pub/files/foobar.txt"},

                // tests 4..7
                {"file:///c:/pub/files/foobar.txt",
                 "file:/c:/pub/files/foobar.txt",
                 "",
                 "/c:/pub/files/foobar.txt"},

                // tests 8..11
                {"file://hpjavaux/c:/pub/files/foobar.txt",
                 "file://hpjavaux/c:/pub/files/foobar.txt",
                 "hpjavaux",
                 "/c:/pub/files/foobar.txt"},

                // tests 12..15
                {"file://c:/pub/files/foobar.txt",
                 "file://c:/pub/files/foobar.txt",
                 "c",
                 "/pub/files/foobar.txt"},

                // tests 16..19
                {"file:/c:/pub/files/foobar.txt",
                 "file:/c:/pub/files/foobar.txt",
                 "",
                 "/c:/pub/files/foobar.txt"},

                // tests 20..23
                {"file:c:/pub/files/foobar.txt",
                 "file:c:/pub/files/foobar.txt",
                 "",
                 "c:/pub/files/foobar.txt"},

                // tests 24..27
                {"file:////hpjavant/bgee/foobar.txt",
                 "file:////hpjavant/bgee/foobar.txt",
                 "",
                 "//hpjavant/bgee/foobar.txt"},

                // tests 28..31
                {"file:///hpjavant/bgee/foobar.txt",
                 "file:/hpjavant/bgee/foobar.txt",
                 "",
                 "/hpjavant/bgee/foobar.txt"},

                // tests 32..35
                {"file://hpjavant/bgee/foobar.txt",
                 "file://hpjavant/bgee/foobar.txt",
                 "hpjavant",
                 "/bgee/foobar.txt"},

                // tests 36..39
                {"file:/hpjavant/bgee/foobar.txt",
                 "file:/hpjavant/bgee/foobar.txt",
                 "",
                 "/hpjavant/bgee/foobar.txt"},

                // tests 40..43
                {"file://hpjavaux//hpjavant/bgee/foobar.txt",
                 "file://hpjavaux//hpjavant/bgee/foobar.txt",
                 "hpjavaux",
                 "//hpjavant/bgee/foobar.txt"},

                // tests 44..47
                {"file://hpjavaux/bgee/foobar.txt",
                 "file://hpjavaux/bgee/foobar.txt",
                 "hpjavaux",
                 "/bgee/foobar.txt"},

                // tests 48..51
                {"file://hpjavaux/c:/pubs/files/foobar.txt",
                 "file://hpjavaux/c:/pubs/files/foobar.txt",
                 "hpjavaux",
                 "/c:/pubs/files/foobar.txt"},

                // tests 52..55
                {"file://bg710571//hpjavant/bgee/foobar.txt",
                 "file://bg710571//hpjavant/bgee/foobar.txt",
                 "bg710571",
                 "//hpjavant/bgee/foobar.txt"},

                // tests 56..59
                {"file://bg710571/bgee/foobar.txt",
                 "file://bg710571/bgee/foobar.txt",
                 "bg710571",
                 "/bgee/foobar.txt"},

                // tests 60..63
                {"file://bg710571/c:/pubs/files/foobar.txt",
                 "file://bg710571/c:/pubs/files/foobar.txt",
                 "bg710571",
                 "/c:/pubs/files/foobar.txt"},
            };

            harness.checkPoint("new URL(string)");
            for (int i = 0; i < s.length; ++i) {
               try {
                    URL url = new URL(s[i][0]);
                    harness.check(url.toExternalForm(), s[i][1]);
                    harness.check(url.getHost(), s[i][2]);
                    harness.check(url.getFile(), s[i][3]);
                }
                catch (Throwable e) {
                    harness.fail("Should not have thrown exception");
		    e.printStackTrace(System.out);
                }
            }
        }

        public void test_cr601b() {
            String[][] s = {

                // tests 0..3
                {"////", "c:/pub/files/foobar.txt",
                 "file://////c:/pub/files/foobar.txt",
                 "////",
                 "c:/pub/files/foobar.txt"},

                 // tests 4..7
                {"///", "c:/pub/files/foobar.txt",
                 "file://///c:/pub/files/foobar.txt",
                 "///",
                 "c:/pub/files/foobar.txt"},

                 // tests 8..11
                {"//", "c:/pub/files/foobar.txt",
                 "file:////c:/pub/files/foobar.txt",
                 "//",
                 "c:/pub/files/foobar.txt"},

                 // tests 12..15
                {"/", "c:/pub/files/foobar.txt",
                 "file:///c:/pub/files/foobar.txt",
                 "/",
                 "c:/pub/files/foobar.txt"},

                 // tests 16..19
                {"", "c:/pub/files/foobar.txt",
                 "file:c:/pub/files/foobar.txt",
                 "",
                 "c:/pub/files/foobar.txt"},

                 // tests 20..23
                {"hpjavaux", "c:/pub/files/foobar.txt",
                 "file://hpjavauxc:/pub/files/foobar.txt",
                 "hpjavaux",
                 "c:/pub/files/foobar.txt"},

                 // tests 24..27
                {null, "c:/pub/files/foobar.txt",
                 "file:c:/pub/files/foobar.txt",
                 null,
                 "c:/pub/files/foobar.txt"},

                 // tests 28..31
                {"////", "//hpjavant/bgee/foobar.txt",
                 "file:////////hpjavant/bgee/foobar.txt",
                 "////",
                 "//hpjavant/bgee/foobar.txt"},

                 // tests 32..35
                {"///", "//hpjavant/bgee/foobar.txt",
                 "file:///////hpjavant/bgee/foobar.txt",
                 "///",
                 "//hpjavant/bgee/foobar.txt"},

                 // tests 36..39
                {"//", "//hpjavant/bgee/foobar.txt",
                 "file://////hpjavant/bgee/foobar.txt",
                 "//",
                 "//hpjavant/bgee/foobar.txt"},

                 // tests 40..43
                {"/", "//hpjavant/bgee/foobar.txt",
                 "file://///hpjavant/bgee/foobar.txt",
                 "/",
                 "//hpjavant/bgee/foobar.txt"},

                 // tests 44..47
                {"", "//hpjavant/bgee/foobar.txt",
                 "file:////hpjavant/bgee/foobar.txt",
                 "",
                 "//hpjavant/bgee/foobar.txt"},

                 // tests 48..51
                {"hpjavaux", "//hpjavant/bgee/foobar.txt",
                 "file://hpjavaux//hpjavant/bgee/foobar.txt",
                 "hpjavaux",
                 "//hpjavant/bgee/foobar.txt"},

                 // tests 52..55
                {null, "//hpjavant/bgee/foobar.txt",
                 "file:////hpjavant/bgee/foobar.txt",
                 null,
                 "//hpjavant/bgee/foobar.txt"},

                 // tests 56..59
                {"hpjavant", "/bgee/foobar.txt",
                 "file://hpjavant/bgee/foobar.txt",
                 "hpjavant",
                 "/bgee/foobar.txt"},

                 // tests 60..63
                {"hpjavant", "/home/bgee/foobar.txt",
                 "file://hpjavant/home/bgee/foobar.txt",
                 "hpjavant",
                 "/home/bgee/foobar.txt"},

                 // tests 64..67
                {"hpjavaux", "/home/bgee/foobar.txt",
                 "file://hpjavaux/home/bgee/foobar.txt",
                 "hpjavaux",
                 "/home/bgee/foobar.txt"},
		
		 // 68..71
		{"hpjavaux", "c:\\foobar.txt",
		 "file://hpjavauxc:\\foobar.txt",
		 "hpjavaux",
		 "c:\\foobar.txt"},
	    };
            harness.checkPoint("new URL(protocol, host, file)");
            for (int i = 0; i < s.length; ++i) {
               try {
                    URL url = new URL("file", s[i][0], s[i][1]);
                    harness.check(url.toExternalForm(), s[i][2]);
                    harness.check(url.getHost(), s[i][3]);
                    harness.check(url.getFile(), s[i][4]);
                    harness.check(true);
                }
                catch (NullPointerException e) {
                    if ((i != 6) && (i != 13)) {
			harness.fail("Should not have thrown NullPointerException");
			e.printStackTrace(System.out);
                    }
                }
                catch (Throwable e) {
		    harness.fail("Should not have thrown exception");
		    e.printStackTrace(System.out);
                }
            }
        }

        public void test_authority()
        {
	    String[][] s = {
		{ "http://sources.redhat.com/",
		  "sources.redhat.com"
		},
		{ "http://user:passwd@sources.redhat.com/",
		  "user:passwd@sources.redhat.com"
		},
		{ "http://sources.redhat.com:90/",
		  "sources.redhat.com:90"
		}
	    };

	    harness.checkPoint("Check for authority support");
	    for (int i = 0; i < s.length; i++)
	      {
		try {
		  URL url = new URL(s[i][0]);
		  
		  harness.check(url.getAuthority(), s[i][1]);
		}
		catch (Throwable t)
		  {
		    harness.fail("Should not have thrown exception");
		    t.printStackTrace(System.out);
		  }
	      }
	}

    public void test_contextResolution() {
	harness.checkPoint("contextResolution");
	try {
	    String[][] testData = new String[][] {
		{"file://www.example.com/foo/bar.txt",
		 "../test.txt",
		 "file://www.example.com/test.txt"
		},
		{"file://www.example.com/foo/bar.txt",
		 "./test.txt",
		 "file://www.example.com/foo/test.txt"
		},
		{"http://www.example.com/foo/bar.txt",
		 "../test.txt",
		 "http://www.example.com/test.txt"
		},
		{"http://www.example.com/foo/bar.txt",
		 "./test.txt",
		 "http://www.example.com/foo/test.txt"
		},
		{"jar:file://www.example.com/test.jar!/foo/bar.txt",
		 "../test.txt",
		 "jar:file://www.example.com/test.jar!/test.txt"
		},
		{"jar:file://www.example.com/test.jar!/foo/bar.txt",
		 "./test.txt",
		 "jar:file://www.example.com/test.jar!/foo/test.txt"
		},
	    };
	    
	    for (int count = 0; count < testData.length; count++) {
		URL base = new URL(testData[count][0]);
		String relative = testData[count][1];
		URL resolved = new URL(base, relative);
		harness.check(resolved.toString(), testData[count][2]);
	    }
	}
	catch (Exception e) {
	    harness.debug(e);
	    harness.fail("Should not have thrown exception");
	}
    }


	public void testall()
	{
		harness.debug("Running: test_Basics");
		test_Basics();
		harness.debug("Running: test_openConnection");
		test_openConnection();
		harness.debug("Running: test_openStream");
		test_openStream();
		harness.debug("Running: test_sameFile");
		test_sameFile();
		harness.debug("Running: test_toString");
		test_toString();
		harness.debug("Running: test_URLStreamHandler");
		test_URLStreamHandler();
		harness.debug("Running: cr601a");
                test_cr601a();
		harness.debug("Running: cr601b");
                test_cr601b();
		harness.debug("Running: authority");
		test_authority();
		harness.debug("Running: test_contextResolution");
		test_contextResolution();
	}

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }

}
