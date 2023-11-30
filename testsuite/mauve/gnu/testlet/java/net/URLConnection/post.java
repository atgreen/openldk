/* post.java --
   Copyright (c) 2004 by Free Software Foundation, Inc.
   Written by Michael Koch <konqueror@gmx.de>

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

public class post implements Testlet
{
  public void test(TestHarness h)
  {
    try
      {
	URL url = new URL("http://sources.redhat.com/mauve/testarea/index.html");
	
	HttpURLConnection conn = (HttpURLConnection) url.openConnection();
	conn.setDoOutput(true); // overwrite default.
	
	h.check(conn.getRequestMethod(), "GET", "request method is 'GET'");

	conn.getOutputStream();
	
	h.check(conn.getRequestMethod(), "POST", "request method is 'POST'");
      }
    catch (MalformedURLException e)
      {
	h.fail("Error in test header - Exception " + e);
      }
    catch (IOException e)
      {
	h.fail("IO error caught - " + e);
      }
  }
}
