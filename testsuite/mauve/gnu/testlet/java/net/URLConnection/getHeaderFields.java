/* getHeaderFields.java --
   Copyright (c) 2004, 2006 by Free Software Foundation, Inc.
   Written by Michael Koch <konqueror@gmx.de>
   Written by Wolfgang Baer <WBaer@gmx.de>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation, version 2. (see COPYING)

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   51 Franklin Street, Fifth Floor, Boston, MA, 02110-1301 USA. */

package gnu.testlet.java.net.URLConnection;

import gnu.testlet.ResourceNotFoundException;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.File;
import java.io.IOException;
import java.net.*;
import java.util.*;

/**
 * Tests getHeaderFields for the various protocol implementations.
 */
public class getHeaderFields implements Testlet
{
  private void check(TestHarness h, String urlString)
  {
    try
      {
	URL url = new URL(urlString);
	URLConnection conn = url.openConnection();
	conn.connect();

	Map headers = conn.getHeaderFields();
    
	h.check(headers != null);

	if (! headers.isEmpty())
	  {
	    Map.Entry entry = (Map.Entry) headers.entrySet().toArray()[1];
	    h.check(entry.getKey() instanceof String);
	    h.check(entry.getValue() instanceof List);
	  }
      }
    catch (MalformedURLException e)
      {
	// Ignored. We know the URIs are valid.
      }
    catch (IOException e)
      {
	h.fail("Test failed for " + urlString);
      }
  }
  
  public void test(TestHarness h)
  {
    // Returns a map of headers.
    h.checkPoint("Test HTTP");
    check(h, "http://www.gnu.org/");

    // Returns an empty map.
    h.checkPoint("Test FTP");
    check(h, "ftp://ftp.gnu.org");
        
    // Returns a map of headers.
    h.checkPoint("Test HTTPS");
    check(h, "https://www.gmx.net/");
    
    try
      {
        // Returns an empty map.
        h.checkPoint("Test JAR");
        File jarfile = h.getResourceFile("gnu#testlet#java#util#jar#JarFile#jartest.jar");    
        check(h, "jar:file:" + jarfile.toString() + "!/");
        
        h.checkPoint("Test File");
        check(h, "file://" + jarfile.toString());
      }
    catch (ResourceNotFoundException e)
      {
        h.debug("Unexpected exception");
        h.debug(e);
      }
  }
}
