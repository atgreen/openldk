// Tags: JDK1.4

// Copyright (C) 2005 Free Software Foundation, Inc.
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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301 USA.


package gnu.testlet.java.io.File;

import java.io.IOException;
import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class newFileURI implements Testlet
{
  public void test (TestHarness harness)
  {
    try
      {
	File file = new File(".");
	URI uri = file.toURI();
	
	File urifile = new File(uri);
	// Check that we get back the original (absolute) file.
	harness.check(urifile, file.getAbsoluteFile());
	
	boolean nullthrown = false;
	try
	  {
	    new File((URI) null);
	  }
	catch (NullPointerException npe)
	  {
	    nullthrown = true;
	  }
	harness.check(nullthrown);
	
	boolean illegalthrown = false;
	try
	  {
	    new File(new URI("ftp://ftp.gnu.org/gnu/classpath"));
	  }
	catch(IllegalArgumentException iae)
	  {
	    illegalthrown = true;
	  }
	harness.check(illegalthrown);

	// Current dir
	harness.check(new File("").getCanonicalFile(),
		      new File(".").getCanonicalFile());
	// Non-hierarchical URI
	try
	  {
		harness.checkPoint("non-hierarchical URI");
		uri = new URI("file:./");
		urifile = new File(uri);
		harness.check(false);
      }
	catch (IllegalArgumentException _)
	  {
		// Expected.
		harness.check(true);
	  }
      }
    catch (IOException ioe)
      {
	harness.debug(ioe);
	harness.check(false, ioe.toString());
      }
    catch (URISyntaxException use)
      {
	harness.debug(use);
	harness.check(false, use.toString());
      }
  }
}

