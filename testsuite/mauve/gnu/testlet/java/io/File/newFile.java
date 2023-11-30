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

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class newFile implements Testlet
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

	File samedir = new File(tmp + File.separator + "mauve-testdir"
				+ File.separator);
	File againdir = new File(tmp + File.separator + "mauve-testdir"
				+ File.separator + File.separator);
	File dirdir = new File(tmp, "mauve-testdir");

	harness.check(tmpdir.isDirectory(),
		      "isDirectory() without separator");
	harness.check(samedir.isDirectory(),
		      "isDirectory() with separator");
	harness.check(againdir.isDirectory(),
		      "isDirectory() with double separators");
	harness.check(dirdir.isDirectory(),
		      "isDirectory() with dir in dir");

	harness.check(tmpdir.getPath(), samedir.getPath(),
		      "dir getPath() with/without trailing separator");
	harness.check(samedir.getPath(), againdir.getPath(),
		      "dir getPath() with (double) trailing separator");
	harness.check(againdir.getPath(), dirdir.getPath(),
		      "dir getPath() with double separator and dir in dir");

	harness.check(tmpdir.getName(), samedir.getName(),
		      "dir getName() with/without trailing separator");
	harness.check(samedir.getName(), againdir.getName(),
		      "dir getName() with (double) separator");
	harness.check(againdir.getName(), dirdir.getName(),
		      "dir getName() with double separator and dir in dir");

	harness.check(tmpdir.getParent(), samedir.getParent(),
		      "same parent with/without separator");
	harness.check(samedir.getParent(), againdir.getParent(),
		      "same parent with (double) separator");
	harness.check(againdir.getParent(), dirdir.getParent(),
		      "same parent with double separator and dir in dir");
	
	harness.checkPoint("getname returns the argument");
	harness.check(new File("dir").getName(), "dir");
	// Directory separator is stripped.
	harness.check(new File("dir" + File.separator).getName(), "dir");
	// If the file separator is backslash, I think it doesn't
	// make sense to check this one.
	if (File.separatorChar != '\\')
		harness.check(new File("dir\\").getName(), "dir\\");
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

