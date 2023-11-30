// Tags: JDK1.4

/*
   Copyright (C) 2005 Michael Koch <konqueror@gmx.de>

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

package gnu.testlet.java.net.URI;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.net.*;

public class URITest implements Testlet
{
  public void testOne(TestHarness h, String uriname,
		      String authority, // 1
		      String fragment, // 2
		      String host, // 3
		      String path, // 4
		      int port, // 5
		      String query, // 6
		      String rawauthority, // 7
		      String rawfragment, // 8
		      String rawpath, // 9
		      String rawquery, // 10
		      String rawschemespecificpart, // 11
		      String rawuserinfo, // 12
		      String scheme, // 13
		      String schemespecificpart, // 14
		      String userinfo, // 15
		      String result) // 16
  {
    try
      {
	h.checkPoint(uriname);
	URI uri = new URI(uriname);

	h.check(uri.getAuthority(), authority); // 1
	h.check(uri.getFragment(), fragment); // 2
	h.check(uri.getHost(), host); // 3
	h.check(uri.getPath(), path); // 4
	h.check(uri.getPort(), port); // 5
	h.check(uri.getQuery(), query); // 6
	h.check(uri.getRawAuthority(), rawauthority); // 7
	h.check(uri.getRawFragment(), rawfragment); // 8
	h.check(uri.getRawPath(), rawpath); // 9
	h.check(uri.getRawQuery(), rawquery); // 10
	h.check(uri.getRawSchemeSpecificPart(), rawschemespecificpart); // 11
	h.check(uri.getRawUserInfo(), rawuserinfo); // 12
	h.check(uri.getScheme(), scheme); // 13
	h.check(uri.getSchemeSpecificPart(), schemespecificpart); // 14
	h.check(uri.getUserInfo(), userinfo); // 15
	h.check(uri.toString(), result); // 16
      }
    catch (URISyntaxException e)
      {
	h.debug(e);
	h.fail("unexpected exception");
      }
  }

  public void test(TestHarness h)
  {
    testOne(h, "mauve://user:passwd@hostname:1234/path/to/file?query=value#fragment",
	    "user:passwd@hostname:1234", // 1
	    "fragment", // 2
	    "hostname", // 3
	    "/path/to/file", // 4
	    1234, // 5
	    "query=value", // 6
	    "user:passwd@hostname:1234", // 7
	    "fragment", // 8
	    "/path/to/file", // 9
	    "query=value", // 10
	    "//user:passwd@hostname:1234/path/to/file?query=value", // 11
	    "user:passwd", // 12
	    "mauve", // 13
	    "//user:passwd@hostname:1234/path/to/file?query=value", // 14
	    "user:passwd", // 15
	    "mauve://user:passwd@hostname:1234/path/to/file?query=value#fragment"); // 16

    testOne(h, "g:h",
	    null, // 1
	    null, // 2
	    null, // 3
	    null, // 4
	    -1, // 5
	    null, // 6
	    null, // 7
	    null, // 8
	    null, // 9
	    null, // 10
	    "h", // 11
	    null, // 12
	    "g", // 13
	    "h", // 14
	    null, // 15
	    "g:h"); // 16

    testOne(h, "g",
	    null, // 1
	    null, // 2
	    null, // 3
	    "g", // 4
	    -1, // 5
	    null, // 6
	    null, // 7
	    null, // 8
	    "g", // 9
	    null, // 10
	    "g", // 11
	    null, // 12
	    null, // 13
	    "g", // 14
	    null, // 15
	    "g"); // 16
	
    testOne(h, "./g",
	    null, // 1
	    null, // 2
	    null, // 3
	    "./g", // 4
	    -1, // 5
	    null, // 6
	    null, // 7
	    null, // 8
	    "./g", // 9
	    null, // 10
	    "./g", // 11
	    null, // 12
	    null, // 13
	    "./g", // 14
	    null, // 15
	    "./g"); // 16

    testOne(h, "g/",
	    null, // 1
	    null, // 2
	    null, // 3
	    "g/", // 4
	    -1, // 5
	    null, // 6
	    null, // 7
	    null, // 8
	    "g/", // 9
	    null, // 10
	    "g/", // 11
	    null, // 12
	    null, // 13
	    "g/", // 14
	    null, // 15
	    "g/"); // 16

    testOne(h, "/g",
	    null, // 1
	    null, // 2
	    null, // 3
	    "/g", // 4
	    -1, // 5
	    null, // 6
	    null, // 7
	    null, // 8
	    "/g", // 9
	    null, // 10
	    "/g", // 11
	    null, // 12
	    null, // 13
	    "/g", // 14
	    null, // 15
	    "/g"); // 16

    testOne(h, "//g",
	    "g", // 1
	    null, // 2
	    "g", // 3
	    "", // 4
	    -1, // 5
	    null, // 6
	    "g", // 7
	    null, // 8
	    "", // 9
	    null, // 10
	    "//g", // 11
	    null, // 12
	    null, // 13
	    "//g", // 14
	    null, // 15
	    "//g"); // 16

    testOne(h, "?y",
	    null, // 1
	    null, // 2
	    null, // 3
	    "", // 4
	    -1, // 5
	    "y", // 6
	    null, // 7
	    null, // 8
	    "", // 9
	    "y", // 10
	    "?y", // 11
	    null, // 12
	    null, // 13
	    "?y", // 14
	    null, // 15
	    "?y"); // 16

    testOne(h, "g?y",
	    null, // 1
	    null, // 2
	    null, // 3
	    "g", // 4
	    -1, // 5
	    "y", // 6
	    null, // 7
	    null, // 8
	    "g", // 9
	    "y", // 10
	    "g?y", // 11
	    null, // 12
	    null, // 13
	    "g?y", // 14
	    null, // 15
	    "g?y"); // 16

    testOne(h, "#s",
	    null, // 1
	    "s", // 2
	    null, // 3
	    "", // 4
	    -1, // 5
	    null, // 6
	    null, // 7
	    "s", // 8
	    "", // 9
	    null, // 10
	    "", // 11
	    null, // 12
	    null, // 13
	    "", // 14
	    null, // 15
	    "#s"); // 16

    testOne(h, "g#s",
	    null, // 1
	    "s", // 2
	    null, // 3
	    "g", // 4
	    -1, // 5
	    null, // 6
	    null, // 7
	    "s", // 8
	    "g", // 9
	    null, // 10
	    "g", // 11
	    null, // 12
	    null, // 13
	    "g", // 14
	    null, // 15
	    "g#s"); // 16

    testOne(h, "g?y#s",
	    null, // 1
	    "s", // 2
	    null, // 3
	    "g", // 4
	    -1, // 5
	    "y", // 6
	    null, // 7
	    "s", // 8
	    "g", // 9
	    "y", // 10
	    "g?y", // 11
	    null, // 12
	    null, // 13
	    "g?y", // 14
	    null, // 15
	    "g?y#s"); // 16

    testOne(h, ";x",
	    null, // 1
	    null, // 2
	    null, // 3
	    ";x", // 4
	    -1, // 5
	    null, // 6
	    null, // 7
	    null, // 8
	    ";x", // 9
	    null, // 10
	    ";x", // 11
	    null, // 12
	    null, // 13
	    ";x", // 14
	    null, // 15
	    ";x"); // 16

    testOne(h, "g;x",
	    null, // 1
	    null, // 2
	    null, // 3
	    "g;x", // 4
	    -1, // 5
	    null, // 6
	    null, // 7
	    null, // 8
	    "g;x", // 9
	    null, // 10
	    "g;x", // 11
	    null, // 12
	    null, // 13
	    "g;x", // 14
	    null, // 15
	    "g;x"); // 16

    testOne(h, "g;x?y#s",
	    null, // 1
	    "s", // 2
	    null, // 3
	    "g;x", // 4
	    -1, // 5
	    "y", // 6
	    null, // 7
	    "s", // 8
	    "g;x", // 9
	    "y", // 10
	    "g;x?y", // 11
	    null, // 12
	    null, // 13
	    "g;x?y", // 14
	    null, // 15
	    "g;x?y#s"); // 16

    testOne(h, ".",
	    null, // 1
	    null, // 2
	    null, // 3
	    ".", // 4
	    -1, // 5
	    null, // 6
	    null, // 7
	    null, // 8
	    ".", // 9
	    null, // 10
	    ".", // 11
	    null, // 12
	    null, // 13
	    ".", // 14
	    null, // 15
	    "."); // 16

    testOne(h, "./",
	    null, // 1
	    null, // 2
	    null, // 3
	    "./", // 4
	    -1, // 5
	    null, // 6
	    null, // 7
	    null, // 8
	    "./", // 9
	    null, // 10
	    "./", // 11
	    null, // 12
	    null, // 13
	    "./", // 14
	    null, // 15
	    "./"); // 16

    testOne(h, "..",
	    null, // 1
	    null, // 2
	    null, // 3
	    "..", // 4
	    -1, // 5
	    null, // 6
	    null, // 7
	    null, // 8
	    "..", // 9
	    null, // 10
	    "..", // 11
	    null, // 12
	    null, // 13
	    "..", // 14
	    null, // 15
	    ".."); // 16

    testOne(h, "../",
	    null, // 1
	    null, // 2
	    null, // 3
	    "../", // 4
	    -1, // 5
	    null, // 6
	    null, // 7
	    null, // 8
	    "../", // 9
	    null, // 10
	    "../", // 11
	    null, // 12
	    null, // 13
	    "../", // 14
	    null, // 15
	    "../"); // 16

    testOne(h, "../g",
	    null, // 1
	    null, // 2
	    null, // 3
	    "../g", // 4
	    -1, // 5
	    null, // 6
	    null, // 7
	    null, // 8
	    "../g", // 9
	    null, // 10
	    "../g", // 11
	    null, // 12
	    null, // 13
	    "../g", // 14
	    null, // 15
	    "../g"); // 16

    testOne(h, "../..",
	    null, // 1
	    null, // 2
	    null, // 3
	    "../..", // 4
	    -1, // 5
	    null, // 6
	    null, // 7
	    null, // 8
	    "../..", // 9
	    null, // 10
	    "../..", // 11
	    null, // 12
	    null, // 13
	    "../..", // 14
	    null, // 15
	    "../.."); // 16

    testOne(h, "../../",
	    null, // 1
	    null, // 2
	    null, // 3
	    "../../", // 4
	    -1, // 5
	    null, // 6
	    null, // 7
	    null, // 8
	    "../../", // 9
	    null, // 10
	    "../../", // 11
	    null, // 12
	    null, // 13
	    "../../", // 14
	    null, // 15
	    "../../"); // 16

    testOne(h, "../../g",
	    null, // 1
	    null, // 2
	    null, // 3
	    "../../g", // 4
	    -1, // 5
	    null, // 6
	    null, // 7
	    null, // 8
	    "../../g", // 9
	    null, // 10
	    "../../g", // 11
	    null, // 12
	    null, // 13
	    "../../g", // 14
	    null, // 15
	    "../../g"); // 16

    // Classpath regression when running jonas.
    testOne(h, "jrmi://localhost:2000",
	    "localhost:2000", // 1
	    null, // 2
	    "localhost", // 3
	    "", // 4
	    2000, // 5
	    null, // 6
	    "localhost:2000", // 7
	    null, // 8
	    "", // 9
	    null, // 10
	    "//localhost:2000", // 11
	    null, // 12
	    "jrmi", // 13
	    "//localhost:2000", // 14
	    null, // 15
	    "jrmi://localhost:2000"); // 16

    String[] tests = { "/a,b", "/a%2C,b", "/a%2c,b" };
    for (int i = 0; i < tests.length; ++i)
      {
	h.checkPoint(tests[i]);
	boolean ok = false;
	URI uri = null;
	try
	  {
	    uri = new URI(tests[i]);
	    ok = uri.toString().equals(tests[i]);
	  }
	catch (URISyntaxException _)
	  {
	    h.debug(_);
	  }
	h.check(ok);
      }
  }
}
