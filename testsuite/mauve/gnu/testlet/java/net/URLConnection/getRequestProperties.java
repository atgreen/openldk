// Tags: JDK1.4

// Copyright (C) 2006 Free Software Foundation, Inc.
// Contributed by David Daney (ddaney@avtrex.com)

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
// the Free Software Foundation, 51 Franklin Street, Fifth Floor,
// Boston, MA  02110-1301  USA

package gnu.testlet.java.net.URLConnection;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;


import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Map;
import java.util.List;

public class getRequestProperties implements Testlet
{
  public void test (TestHarness harness)
  {
    try
      {
        harness.checkPoint("getRequestProperties");
        URL url = new URL("http://foo.bar/blah/blah");
        URLConnection c = url.openConnection();
        
        c.addRequestProperty("mauve", "p1");
        c.addRequestProperty("mauve", "p2");
        
        Map m = c.getRequestProperties();
        
        List l = (List)m.get("mauve");
        
        harness.check(l.contains("p1"));
        harness.check(l.contains("p2"));

      }
    catch (ClassCastException cce)
      {
	harness.debug(cce);
	harness.fail("ClassCastException");
      }
    catch (IOException ioe)
      {
	harness.debug(ioe);
	harness.fail("IOException");
      }
    catch (Exception e)
      {
	harness.debug(e);
	harness.fail("Unexpected Exception");
      }
  }
}
