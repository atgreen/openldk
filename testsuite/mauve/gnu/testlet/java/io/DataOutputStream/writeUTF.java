// Tags: JDK1.0

// Copyright (C) 2002 Free Software Foundation, Inc.
// Written by Mark Wielaard

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

package gnu.testlet.java.io.DataOutputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.io.*;

public class writeUTF implements Testlet
{
  TestHarness harness;

  public void test (TestHarness harness)
  {
    this.harness = harness;
   
    try
      {
	ByteArrayOutputStream baos = new ByteArrayOutputStream();
	DataOutputStream dos = new DataOutputStream(baos);
	dos.writeUTF("\u0000"
		     + "\u0001\u0002\u007e\u007f"
		     + "\u0080\u0081\u07fe\u07ff"
		     + "\u0800\u0801\ufffe\uffff");
	dos.close();
	byte[] bs = baos.toByteArray();
	byte[] encoded = {(byte)0x00, (byte)0x1a, // size (26)
			  (byte)0xc0, (byte)0x80, // \u0000
			  (byte)0x01, // \u0001
			  (byte)0x02, // \u0002
			  (byte)0x7e, // \u007e
			  (byte)0x7f, // \u007f
			  (byte)0xc2, (byte)0x80, // \u0080
			  (byte)0xc2, (byte)0x81, // \u0081
			  (byte)0xdf, (byte)0xbe, // \u07fe
			  (byte)0xdf, (byte)0xbf, // \u07ff
			  (byte)0xe0, (byte)0xa0, (byte)0x80,  // \u0800
			  (byte)0xe0, (byte)0xa0, (byte)0x81,  // \u0801
			  (byte)0xef, (byte)0xbf, (byte)0xbe,  // \ufffe
			  (byte)0xef, (byte)0xbf, (byte)0xbf}; // \uffff
	checkArrayEquals(bs, encoded);
      }
    catch (IOException ioe)
      {
	harness.fail("Unexpected IOException: " + ioe);
      }
  }
  
  private void checkArrayEquals(byte[] b1, byte[] b2)
  {
    int length = b1.length;
    if (length != b2.length)
      {
	harness.debug("b1.length=" + length
		      + ", but b2.length=" + b2.length);
	harness.fail("arrays same");
	return;
      }

    for (int i = 0; i < length; i++)
      if (b1[i] != b2[i])
	{
	  harness.debug("b1[" + i + "] = " + b1[i]
			+ ", but b2[" + i + "] = " + b2[i]);
	  harness.fail("arrays not equal");
	  return;
	}

    harness.check(true, "arrays same");
  }
}
