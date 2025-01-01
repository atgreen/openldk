// Tags: JDK1.4

// Copyright (C) 2004 Free Software Foundation, Inc.
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

package gnu.testlet.java.nio.channels.FileChannel;

import java.io.*;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Naive test for opening (but never closing) a large number of
 * FileChannels.
 */
public class manyopen implements Testlet
{
  private final int MANY = 1024;
  public void test (TestHarness harness)
  {
    Runtime runtime = Runtime.getRuntime();
    String tmpfile = harness.getTempDirectory()
	    + File.separator + "mauve-many.";
    int i = 0;
    try
      {
	for (i = 0; i < MANY; i++)
	  {
	    File f = new File(tmpfile + i + ".in");
	    f.createNewFile();
	    FileInputStream fis = new FileInputStream(f);

	    f = new File(tmpfile + i + ".out");
	    FileOutputStream fos = new FileOutputStream(f);

	    f = new File(tmpfile + i + ".raf");
	    RandomAccessFile raf = new RandomAccessFile(f, "rw");
	  }
	harness.check(true);
      }
    catch(IOException ioe)
      {
	harness.fail("Unexpected exception at nr " + i + ": " + ioe);
	harness.debug(ioe);
      }
    finally
      {
	// Cleanup
	for (i = 0; i < MANY; i++)
	  {
	    File f = new File(tmpfile + i + ".in");
	    f.delete();

	    f = new File(tmpfile + i + ".out");
	    f.delete();

	    f = new File(tmpfile + i + ".raf");
	    f.delete();
	  }
      }
  }
}

