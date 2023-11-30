// Copyright (c) 2005  Red Hat, Inc.
// Written by Ito Kazumitsu <kaz@maczuka.gcd.org>

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

package gnu.testlet.java.io.PrintStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.io.*;

public class encodings implements Testlet
{

  private void test1 (TestHarness harness, String encoding,
		      String input, byte[] expected)
  {
    byte[] output = null;
    try
      {
	ByteArrayOutputStream b = new ByteArrayOutputStream ();
	PrintStream ps = null;
	if (encoding == null)
	  {
	    ps = new PrintStream(b, false);
	  }
	else
	  {
	    ps = new PrintStream(b, false, encoding);
	  }
	ps.print(input);
	ps.flush();
	output = b.toByteArray();
      }
    catch (UnsupportedEncodingException e)
      {
      }
    if (output == null && expected == null)
      {
	harness.check(true);
	return;
      }
    boolean result = (output != null && output.length == expected.length);
    if (result)
      {
	for (int i = 0; i < output.length; i++)
	  {
	    if (output[i] != expected[i])
	      {
	        result = false;
	        break;
	      }
	  }
      }
    harness.check(result);
  }

  public void test (TestHarness harness)
  {
    String input = "abc";
    byte[] expected = new byte[] {(byte)'a', (byte)'b', (byte)'c'};

    test1 (harness, "ISO-8859-1", input, expected);
    test1 (harness, "??UNSUPPORTED??", input, null);
   /*
      The result of setting the system property "file.encoding"
      is uncertain.
    String saved_encoding = System.getProperty("file.encoding");
    System.setProperty ("file.encoding", "ISO-8859-1");
    test1 (harness, null, input, expected);
    System.setProperty ("file.encoding", "??UNSUPPORTED??");
    test1 (harness, null, input, expected);
    System.setProperty("file.encoding", saved_encoding);
    */
  }

}
