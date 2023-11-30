/* InputTest.java -- Tests HTTPConnection behaviour

   Copyright (c) 2003 by Free Software Foundation, Inc.
   Written by Guilhem Lavaux <guilhem@kaffe.org>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation, version 2. (see COPYING)

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation
   Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA */
package gnu.testlet.java.net.URLConnection;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import java.net.*;
import java.io.IOException;

public class Http implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("Good HTTP header ordering");
    try
      {
	URL url = new URL("http://sources.redhat.com/mauve/testarea/index.html");
	URLConnection conn = url.openConnection();
	
	harness.check(conn.getHeaderFieldKey(0), null);
	harness.check(conn.getHeaderField(0), "HTTP/1.1 200 OK");

	harness.check(conn.getHeaderFieldKey(2), "Server");
	harness.check(conn.getHeaderField(2).indexOf("Apache"), 0);
	
      }
    catch (MalformedURLException e)
      {
	harness.fail("Error in test header - Exception " + e);
      }
    catch (IOException e)
      {
	harness.fail("IO error caught - " + e);
      }
  }
}
