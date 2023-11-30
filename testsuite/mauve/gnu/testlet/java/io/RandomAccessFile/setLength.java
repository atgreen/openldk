// Tags: JDK1.2

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

package gnu.testlet.java.io.RandomAccessFile;

import java.io.*;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class setLength implements Testlet
{
  public void test (TestHarness harness)
  {
    String tmpfile = harness.getTempDirectory()
	    + File.separator + "mauve-raf.tst";
    File f = new File(tmpfile);
    f.delete();
    try
      {
	RandomAccessFile raf = new RandomAccessFile(f, "rw");
	harness.check(raf.length(), 0);
	harness.check(raf.getFilePointer(), 0);
	raf.write(new byte[] {1, 2, 3, 4, 5, 6, 7, 8} );
	harness.check(raf.length(), 8);
	harness.check(raf.getFilePointer(), 8);

	// Truncate
	raf.setLength(3);
	harness.check(raf.length(), 3);
	harness.check(raf.getFilePointer(), 3);

	// End of file
	harness.check(raf.read(), -1);
	harness.check(3, raf.length());

	// Expand
	raf.write(10);
	harness.check(raf.length(), 4);
	harness.check(raf.getFilePointer(), 4);

	// Expand with setLength
	raf.setLength(10);
	harness.check(raf.length(), 10);
	harness.check(raf.getFilePointer(), 4);

	// Truncate with setLength
	raf.setLength(5);
	harness.check(raf.length(), 5);
	harness.check(raf.getFilePointer(), 4);

	// Truncate with setLength before file position
	raf.setLength(1);
	harness.check(raf.length(), 1);
	harness.check(raf.getFilePointer(), 1);
      }
    catch(IOException ioe)
      {
	harness.fail("Unexpected: " + ioe);
	harness.debug(ioe);
      }
    finally
      {
	// Cleanup
	f.delete();
      }
  }
}

