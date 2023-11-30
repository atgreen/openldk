// Tags: JDK1.2
// Uses: getResourceBase

// Copyright (C) 2002 Free Software Foundation, Inc.
// Written by Mark Wielaard (mark@klomp.org)

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

package gnu.testlet.java.net.URLClassLoader;

import java.io.IOException;

import java.net.URL;
import java.net.URLClassLoader;

import gnu.testlet.TestHarness;

/**
 * Version of getResource test that uses a remote net connection.
 */
public class getResourceRemote extends getResourceBase
{
  public void test (TestHarness h)
  {
    harness = h;

    try
      {
	URL[] urls = new URL[2];
	urls[0] = new URL
	  ("http://sources.redhat.com/mauve/testarea/");
	urls[1] = new URL
	  ("http://sources.redhat.com/mauve/testarea/remotejar.jar");
	ucl = URLClassLoader.newInstance(urls);
	
	check("testresource", "/mauve/testarea", true);
	check("testdir/resourceindir", "/mauve/testarea", true);
	check("remote-jresource", "/mauve/testarea/remotejar.jar",  false);
	check("path/in/remote-jar/resourcefile",
	      "/mauve/testarea/remotejar.jar", false);
      }
    catch (IOException ioe)
      {
	harness.fail("Unexpected exception: " + ioe);
	harness.debug(ioe);
      }
  }
}
