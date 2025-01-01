// Tags: JDK1.2

/* Jar.java -- Tests Jar URL connection

   Copyright (c) 2005, 2006 by Free Software Foundation, Inc.
   Written by Tom Tromey <tromey@redhat.com>
   Extended by Wolfgang Baer <WBaer@gmx.de>

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

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.File;
import java.io.FileNotFoundException;
import java.net.JarURLConnection;
import java.net.URL;

public class Jar implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("jar: URL with missing entry");
    try
      {
	File jarfile = harness.getResourceFile("gnu#testlet#java#util#jar#JarFile#jartest.jar");
	String filename = jarfile.toString();

	URL url = new URL("jar:file:" + filename + "!/nosuchfile.txt");

        // Test via JarURLConnection
        // FileNotFoundException must already be thrown in connect
        JarURLConnection connection = null;
        try 
          {
            connection = (JarURLConnection) url.openConnection();
            connection.connect();
            harness.check(false);
          }
        catch (FileNotFoundException e)
          {
            harness.check(true);
          }
        catch (Exception e)
          {
            harness.check(false);
          }

        // Test via direct opening of the stream on the URL object
        try
          {
            url.openStream();
            harness.check(false);
          }
        catch (FileNotFoundException e)
          {
            harness.check(true);
          }
        catch (Exception e)
          {
            harness.check(false);
          }
        
      }   
    catch (Throwable e)
      {
        harness.debug("Unexpected exception in testcase.");
	harness.debug(e);
      }
  }
}
