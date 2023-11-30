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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class canWrite implements Testlet
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
        tmpfile = new File(tmpdir, "testfile");
        harness.check(tmpfile.delete() || !tmpfile.exists(), "no temp file");

	harness.check(tmpdir.canRead(), "dir.canWrite()");
	harness.check(tmpdir.canWrite(), "dir.canWrite()");
	harness.check(tmpdir.setReadOnly(), "dir.setReadOnly()");
	harness.check(tmpdir.canRead(), "dir.canWrite() after setReadOnly()");
	harness.check(!tmpdir.canWrite(), "dir.canWrite() after SetReadOnly()");

	harness.check(!tmpfile.canRead(), "non-existing-file.canRead()");
	harness.check(!tmpfile.canWrite(), "non-existing-file.canWrite()");
        harness.check(!tmpfile.setReadOnly(),
		      "non-existing-file.setReadOnly()");

	boolean create;
	try
	  {
	    create = tmpfile.createNewFile();
	  }
	catch (IOException ioe)
	  {
	    create = false;
	  }
	harness.check(!create, "creating file in read only dir");

	// Remove and re-setup
	tmpfile.delete();
	tmpdir.delete();
        tmpdir.mkdir();
	harness.check(tmpdir.canRead(), "dir.canRead() after recreation");
	harness.check(tmpdir.canWrite(), "dir.canWrite() after recreation");

	try
	  {
	    create = tmpfile.createNewFile();
	  }
	catch (IOException ioe)
	  {
	    create = false;
            harness.debug(ioe);
	  }
	harness.check(create, "creating file in new dir");
	harness.check(tmpfile.canRead(), "file.canRead() after recreation");
	harness.check(tmpfile.canWrite(), "file.canWrite() after recreation");

	boolean write;
	OutputStream os = null;
	try
	  {
	    os = new FileOutputStream(tmpfile);
	    os.write(0);
	    write = true;
	  }
	catch(IOException ioe)
	  {
	    write = false;
	    harness.debug(ioe);
	  }
	finally
	  {
	    try
	      {
		if (os != null)
		    os.close();
		os = null;
	      }
	    catch(IOException _)
	      {
	      }
	  }
	harness.check(write, "Actually write to new file");

	harness.check(tmpfile.setReadOnly(), "file.setReadOnly()");

	try
	  {
	    os = new FileOutputStream(tmpfile);
	    os.write(0);
	    write = true;
	  }
	catch(IOException ioe)
	  {
	    write = false;
	  }
	finally
	  {
	    try
	      {
		if (os != null)
		    os.close();
		os = null;
	      }
	    catch(IOException _)
	      {
	      }
	  }
	harness.check(!write, "Write to file after setReadOnly()");
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

