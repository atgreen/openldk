// Test for OutputStream methods

// Written by Daryl Lee (dol@sources.redhat.com)

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
// Boston, MA 02111-1307, USA.

// Tags: JDK1.1

package gnu.testlet.java.io.OutputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.OutputStream;
import java.io.IOException;

public class Test extends OutputStream implements Testlet
{
	private static final int LEN = 100;
	private byte[] buf;
	private int index;

	public Test()
	{
		super();
		buf = new byte[LEN];
		index = 0;
	}

	public final void write(int c) throws IOException
	{
		buf[index++] = (byte) c;
	}

	// a utility method for testing
	public String toString()
	{
		return new String(buf, 0, index);
	}
		
  public void test (TestHarness harness)
  {
    try
      {
		String tstr = "ABCDEFGH";
		Test ts = new Test();
		ts.write(tstr.charAt(0));					// 'A'
		harness.check(true,"write(int)");
		byte[] cbuf = new byte[8];
		tstr.getBytes(0, 8, cbuf, 0);
		ts.write(cbuf, 0, 4);						// 'AABCD'
		harness.check(true,"write(byte[], off, len)");
		ts.write(cbuf);								// 'AABCDABCDEFGH'
		ts.flush();
		harness.check(true, "flush()");
		harness.check(ts.toString(), "AABCDABCDEFGH", "Wrote all characters okay");	
		ts.close ();
		harness.check(true, "close()");

      }
    catch (IOException e)
      {
		harness.check(false, "IOException unexpected");
      }
  }
}
