// Test for InputStreamReader methods

// Written by Daryl Lee (dol@sources.redhat.com)
// Elaboration of except.java  by paul@dawa.demon.co.uk

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

package gnu.testlet.java.io.InputStreamReader;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.io.*;

public class jdk11 implements Testlet
{
  public void test (TestHarness harness)
  {
    try
      {
		InputStreamReader isr = new InputStreamReader (new StringBufferInputStream ("zardoz has spoken"));
		harness.check(isr.ready(), "ready()");   // deprecated post-1.1

		harness.check(isr.getEncoding() != null,
				"non-null getEncoding");

		char[] cbuf = new char[10];
		isr.read (cbuf, 0, cbuf.length);
		String tst = new String(cbuf);
		harness.check(tst, "zardoz has", "read(buf[], off, len)");
		harness.check(isr.read(), ' ', "read()");	
		isr.close ();
		harness.check(isr.getEncoding(), null,
				"null encoding after close");
      }
    catch (IOException e)
      {
		harness.check(false, "IOException unexpected");
      }
  }
}
