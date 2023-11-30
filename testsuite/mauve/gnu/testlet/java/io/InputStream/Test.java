// Test for InputStream methods

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

package gnu.testlet.java.io.InputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.io.InputStream;
import java.io.IOException;

public class Test extends InputStream implements Testlet
{
  private String s;
  private int index;

  public Test() { }

  Test (String str)
  {
    super();
    s = str;
    index = 0;
  }

  public int read() throws IOException
  {
    return(index == s.length() ? -1 : s.charAt(index++));
  }

  public void test (TestHarness harness)
  {
    try
      {
		Test tis = new Test ("zardoz has spoken");
		byte[] cbuf = new byte[10];
		tis.read (cbuf, 0, cbuf.length);
		String tst = new String(cbuf);
		harness.check(tst, "zardoz has", "read(buf[], off, len)");
		harness.check(tis.read(), ' ', "read()");	
		tis.close ();
		harness.check(true, "close()");
      }
    catch (IOException e)
      {
		harness.check(false, "IOException unexpected");
      }
  }
}
