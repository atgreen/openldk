// Tags: JDK1.1

// Copyright (C) 2002 Free Software Foundation, Inc.
// Written by Daryl Lee (dolee@sources.redhat.com)
// Modified from FileOutputStream/write.java,
//     written by Mark Wielaard (mark@klomp.org)

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

package gnu.testlet.java.io.FilterOutputStream;

import java.io.ByteArrayOutputStream;
import java.io.FilterOutputStream;
import java.io.IOException;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class write implements Testlet
{
	class TestOutputStream extends FilterOutputStream
	{
		TestOutputStream (ByteArrayOutputStream baos)
		{
			super(baos);
		}
	}
  public void test (TestHarness harness)
  {
	ByteArrayOutputStream baos = new ByteArrayOutputStream();
	FilterOutputStream fos = new FilterOutputStream(baos);;
	byte[] ba = {(byte)'B', (byte)'C', (byte)'D'};
	try {
		String tststr = "ABCD";
		fos.write('A');
		harness.check(true, "write(int)");
		fos.write(ba);
		harness.check(true, "write(buf)");
		fos.write(ba,0,3);
		harness.check(true, "write(buf,off,len)");
		byte[] finalba = baos.toByteArray();
		String finalstr2 = new String(finalba);
		harness.check(finalstr2.equals("ABCDBCD"), "wrote all characters okay");
		baos.flush();
		harness.check(true, "flush()");
		baos.close();
		harness.check(true, "close()");
	}
	catch (IOException e) {
		harness.fail("IOException unexpected");
	}
  }
}
