// Tags: JDK1.2

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

package gnu.testlet.java.io.File;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class list implements Testlet
{
  File tmpdir;
  File tmpfile;

  public void test (TestHarness harness)
  {
    try
      {
	// Setup
	String tmp = harness.getTempDirectory();
	tmpdir = new File(tmp + File.separator + "mauve-testdir");
	harness.check(tmpdir.mkdir() || tmpdir.exists(), "temp directory");

	String[] list = tmpdir.list();
	harness.check(list.length, 0, "empty directory");
	harness.debug("list.length: " + list.length);
	if (list.length > 0)
	  harness.debug("Unexpected: " + list[0]);
	
	list = tmpdir.list((FilenameFilter)null);
	harness.check(list.length, 0, "empty directory, null filter");
	harness.debug("list.length: " + list.length);
	if (list.length > 0)
	  harness.debug("Unexpected: " + list[0]);
	
	// non-existing file
	tmpfile = new File(tmpdir, "testfile");
	harness.check(tmpfile.delete() || !tmpfile.exists(), "no temp file");

	list = tmpdir.list();
	harness.check(list.length, 0, "no real file in dir");
	harness.debug("list.length: " + list.length);
	if (list.length > 0)
	  harness.debug("Unexpected: " + list[0]);

	list = tmpdir.list((FilenameFilter)null);
	harness.check(list.length, 0, "no real file in dir, null filter");
	harness.debug("list.length: " + list.length);
	if (list.length > 0)
	  harness.debug("Unexpected: " + list[0]);

	list = tmpfile.list();
	harness.check(list, null, "non-existing-file");

	list = tmpfile.list((FilenameFilter)null);
	harness.check(list, null, "non-existing-file, null filter");
	
	// not-a-directory
	tmpfile.createNewFile();
	list = tmpfile.list();
	harness.check(list, null, "not-a-directory");

	list = tmpfile.list((FilenameFilter)null);
	harness.check(list, null, "not-a-directory, null filter");

	// File in directory
	list = tmpdir.list();
	harness.check(list != null
			&& list.length == 1
			&& list[0].equals("testfile"),
		      "one file in directory");

	list = tmpdir.list((FilenameFilter)null);
	harness.check(list != null
			&& list.length == 1
			&& list[0].equals("testfile"),
		      "one file in directory, null filter");

	// For all roots it should give something.
	File[] roots = File.listRoots();
	for (int i = 0; i < roots.length; i++)
	  {
	    harness.check(roots[i].list() != null, "root " + i);
	    harness.check(roots[i].list((FilenameFilter)null) != null,
			    "root " + i + ", null filter");
	  }

      }
    catch(IOException ioe)
      {
	harness.fail("Unexpected exception: " + ioe);
	harness.debug(ioe);
      }
    finally
      {
	// Cleanup
	if (tmpdir != null && tmpdir.exists())
	  {
	    if (tmpfile != null && tmpfile.exists())
	      tmpfile.delete();
	    tmpdir.delete();
	  }
      }
  }
}

