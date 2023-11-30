// Tags: JDK1.4

// Copyright (C) 2005 Free Software Foundation, Inc.
// Written by Ito Kazumitsu (kaz@maczuka.gcd.org)

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
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Naive test for reading from and writing to FileChannels.
 */
public class copyIO implements Testlet
{
  private static final byte[] source = "abcdefghijklmnopqrstuvwxyz".getBytes();
  private static final int BUFSIZE = 10;
  public void test (TestHarness harness)
  {
    File tmpf1 = null;
    File tmpf2 = null;
    try
      {
	byte[] source = "abcdefghijklmnopqrstuvwxyz".getBytes();
    	String tmpfile = harness.getTempDirectory()
	    + File.separator + "mauve-copyIO.";
	tmpf1 = new File(tmpfile + "TEST1");
	tmpf2 = new File(tmpfile + "TEST2");
	FileOutputStream fos = new FileOutputStream(tmpf1);
	fos.write(source);
	fos.close();

	FileInputStream tmpin = new FileInputStream(tmpf1);
	FileOutputStream tmpout = new FileOutputStream(tmpf2);
        FileChannel in = tmpin.getChannel();
        FileChannel out = tmpout.getChannel();

	copyIO(BUFSIZE, in, out);

	tmpin.close();
	tmpout.close();

	FileInputStream fis = new FileInputStream(tmpf2);
	byte[] result = new byte[source.length + 1];
	int l = fis.read(result, 0, result.length);
	if (l == source.length)
	  {
	    harness.check(new String(source).equals(new String(result,0, l)));
	  }
	else
	  {
	    if (l >= 0)
	      {
	    	harness.fail("Unexpected result: source=" + new String(source)
			+ ", result=" + new String(result,0, l));
	      }
	    else
	      {
	    	harness.fail("Unexpected EOF");
	      }
	  }
      }
    catch (Exception e)
      {
	harness.fail("Unexpected exception: " + e);
	harness.debug(e);
      }
    finally
      {
	 if (tmpf1 != null) tmpf1.delete();
	 if (tmpf2 != null) tmpf2.delete();
      }
  }

  private static void copyIO(int bufsize, FileChannel in, FileChannel out)
	throws IOException
  {
    ByteBuffer buffer = ByteBuffer.allocate(bufsize);
    boolean eof = false;
    while (!eof)
      {
	buffer.clear();
        int pos = buffer.position();
        int limit = buffer.limit();
	// Check whether the position is moved forward
	for (int i = pos + 1; i <= limit; i++)
	  {
	    buffer.limit(i);
	    int l = in.read(buffer);
            if (l < 0)
	      {
		eof = true;
		break;
	      }
	  }
        buffer.flip();
        pos = buffer.position();
        limit = buffer.limit();
	// Check whether the position is moved forward
	for (int i = pos + 1; i <= limit; i++)
	  {
	    buffer.limit(i);
            out.write(buffer);
	  }
      }
  }
}
