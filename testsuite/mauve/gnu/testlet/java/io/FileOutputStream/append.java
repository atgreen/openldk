// Tags: JDK1.1

// Copyright (C) 2007 Free Software Foundation, Inc.
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


package gnu.testlet.java.io.FileOutputStream;

import java.io.*;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Based on a testcase submitted by Steve Blackburn
 * (Steve.Blackburn@anu.edu.au) for the following bug report:
 * [ 1722506 ] dacapo eclipse fails EOF exceptions 
 * http://sourceforge.net/tracker/index.php?func=detail&aid=1722506&group_id=128805&atid=712768
 */
public class append
  implements Testlet
{
  public void test (TestHarness harness)
  {
    // Make sure test file is created fresh
    String tmpfile = harness.getTempDirectory()
      + File.separator + "mauve-fos-append.tst";
    File f = new File(tmpfile);
    f.delete();

    try
      {
	// Don't append (large)
	FileOutputStream fos = new FileOutputStream(f, false);
	BufferedOutputStream bos = new BufferedOutputStream(fos, 2048);
	DataOutputStream dos = new DataOutputStream(bos);
	for (int i = 0; i < 50; i++)
	  dos.writeInt(i);
	dos.close();
	long len1 = f.length();
	RandomAccessFile raf = new RandomAccessFile(f, "rw");
	harness.check(raf.length(), len1);
	raf.close();
	dos.close();
	
	// Don't append (small) (truncates)
	fos = new FileOutputStream(f, false);
	bos = new BufferedOutputStream(fos, 2048);
	dos = new DataOutputStream(bos);
	for (int i = 0; i < 25; i++)
	  dos.writeInt(i);
	dos.close();
	long len2 = f.length();
	raf = new RandomAccessFile(f, "rw");
	harness.check(raf.length(), len2);
	raf.close();
	dos.close();

	// Append large (extends)
	fos = new FileOutputStream(f, true);
	bos = new BufferedOutputStream(fos, 2048);
	dos = new DataOutputStream(bos);
	for (int i = 0; i < 50; i++)
	  dos.writeInt(i);
	dos.close();
	long len3 = f.length();
	harness.check(len1 + len2, len3);
	raf = new RandomAccessFile(f, "rw");
	harness.check(raf.length(), len3);
	raf.close();

	// Don't append (small) again (truncates)
	fos = new FileOutputStream(f, false);
	bos = new BufferedOutputStream(fos, 2048);
	dos = new DataOutputStream(bos);
	for (int i = 0; i < 25; i++)
	  dos.writeInt(i);
	dos.close();
	long len4 = f.length();
	harness.check(len2, len4);
	raf = new RandomAccessFile(f, "rw");
	harness.check(raf.length(), len4);
	raf.close();
	dos.close();
      }
    catch (IOException ioe)
      {
	harness.fail("Unexpected: " +ioe);
	harness.debug(ioe);
      }
    finally
      {
	f.delete();
      }
  }
}

