// Tags: JDK1.0

// Contributed by Mark Wielaard (mark@klomp.org)
// Based on a kaffe regression test.

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.net.URL;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.net.MalformedURLException;
import java.net.URL;

public class newURL implements Testlet
{
  private TestHarness harness;

  public void test (TestHarness harness)
  {
    this.harness = harness;

    check(null,
	  "jar:http://www.kaffe.org/foo/bar.jar!/float/boat",
	  "jar:http://www.kaffe.org/foo/bar.jar!/float/boat");
    check(null,
	  "http://www.kaffe.org",
	  "http://www.kaffe.org");
    check(null,
	  "http://www.kaffe.org:8080#ref",
	  "http://www.kaffe.org:8080#ref");
    check("http://www.kaffe.org",
	  "foo/bar",
	  "http://www.kaffe.org/foo/bar");
    check("http://www.kaffe.org/foo/bar#baz",
	  "jan/far",
	  "http://www.kaffe.org/foo/jan/far");
    check("http://www.kaffe.org/foo/bar",
	  "/jan/far",
	  "http://www.kaffe.org/jan/far");
    check("http://www.kaffe.org/foo/bar",
	  "",
	  "http://www.kaffe.org/foo/bar");
    check(null,
	  "foo/bar",
	  null);
    check("file:/foo/bar",
	  "barf#jow",
	  "file:/foo/barf#jow");
    check("file:/foo/bar#fly",
	  "jabawaba",
	  "file:/foo/jabawaba");
    check(null,
      "jar:file:/usr/local/share/kaffe/Klasses.jar!/kaffe/lang/unicode.tbl",
      "jar:file:/usr/local/share/kaffe/Klasses.jar!/kaffe/lang/unicode.tbl");
    check(null,
	  "jar:http://www.kaffe.org/foo/bar.jar",
	  null);
    check("jar:http://www.kaffe.org/foo/bar.jar!/path/name",
	  "float/boat",
	  "jar:http://www.kaffe.org/foo/bar.jar!/path/float/boat");
    check("jar:http://www.kaffe.org/foo/bar.jar!/",
	  "float/boat",
	  "jar:http://www.kaffe.org/foo/bar.jar!/float/boat");
    check("jar:http://www.kaffe.org/foo/bar.jar!/path/name",
	  "/float/boat",
	  "jar:http://www.kaffe.org/foo/bar.jar!/float/boat");
    check("jar:http://www.kaffe.org/foo/bar.jar!/",
	  "/float/boat",
	  "jar:http://www.kaffe.org/foo/bar.jar!/float/boat");
    check("jar:http://www.kaffe.org/foo/bar.jar!/float",
	  "#boat",
	  "jar:http://www.kaffe.org/foo/bar.jar!/float#boat");
    check(null,
	  "http://www.kaffe.org:99999/foo/bar",
	  "http://www.kaffe.org:99999/foo/bar");
    check(null,
	  "jar:abc!/eat/me",
	  null);
    
    URL u = check(null,
		  "http://anonymous:anonymous@host/",
		  "http://anonymous:anonymous@host/");
    harness.check(u.getHost(), "host");
    harness.check(u.getUserInfo(), "anonymous:anonymous");
  }

  // Checks that the URL created from the context plus the url gives
  // the string result. Or when the result is null, whether the
  // contruction throws a exception. Returns the generated URL or null.
  private URL check(String context, String url, String string)
  {
    harness.checkPoint(context + " + " + url + " = " + string);
    URL c;
    if (context != null)
      {
	try
	  {
	    c = new URL(context);
	  }
	catch (MalformedURLException mue)
	  {
	    harness.debug(mue);
	    harness.check(false);
	    return null;
	  }
      }
    else
      c = null;

    try
      {
	URL u = new URL(c, url);
	harness.check(u.toString(), string);
	return u;
      }
    catch (MalformedURLException mue)
      {
	boolean expected = (string == null);
	if (!expected)
	  harness.debug(mue);
	harness.check(expected);
	return null;
      }
  }
}
