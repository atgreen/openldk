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
import java.nio.*;
import java.nio.channels.*;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class truncate implements Testlet
{
  public void test (TestHarness harness)
  {
    String tmpfile = harness.getTempDirectory()
	    + File.separator + "mauve-trunc.tst";
    File f = new File(tmpfile);
    f.delete();
    try
      {
	RandomAccessFile raf = new RandomAccessFile(f, "rw");
	FileChannel fc = raf.getChannel();
	harness.check(fc.size(), 0);
	harness.check(fc.position(), 0);

	ByteBuffer bb;
	bb = ByteBuffer.wrap(new byte[] {1, 2, 3, 4, 5, 6, 7, 8});
	harness.check(fc.write(bb), 8);
	harness.check(fc.size(), 8);
	harness.check(fc.position(), 8);

	// Truncate
	fc.truncate(3);
	harness.check(fc.size(), 3);
	harness.check(fc.position(), 3);

	// End of file
	harness.check(fc.read(ByteBuffer.allocate(1)), -1);
	harness.check(3, fc.size());

	// Expand with write
	bb = ByteBuffer.allocate(1);
	bb.put((byte) 10);
	bb.flip();
	harness.check(fc.write(bb), 1);
	harness.check(fc.size(), 4);
	harness.check(fc.position(), 4);

	// Expand with truncate (shouldn't work)
	fc.truncate(10);
	harness.check(fc.size(), 4);
	harness.check(fc.position(), 4);

	// Set position and truncate just after (end of file)
	fc.position(3);
	fc.truncate(4);
	harness.check(fc.size(), 4);
	harness.check(fc.position(), 3);

	// Truncate before file position and file end
	fc.truncate(1);
	harness.check(fc.size(), 1);
	harness.check(fc.position(), 1);
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

