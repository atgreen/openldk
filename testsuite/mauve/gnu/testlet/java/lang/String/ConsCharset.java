// Tags: JDK1.6

// Copyright (C) 2009 Red Hat, Inc.

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

package gnu.testlet.java.lang.String;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.nio.charset.Charset;
import java.nio.charset.UnsupportedCharsetException;

public class ConsCharset
  implements Testlet
{
  public void test (TestHarness h)
  {
    try
      {
	byte[] cp437Bytes = asByteArray(new int[] { 224, 226, 227, 228, 156 });
	checkString(h, new String(cp437Bytes, Charset.forName("CP437")),
				  "\u03b1\u0393\u03c0\u03a3\u00a3");
      }
    catch (UnsupportedCharsetException e)
      {
	// Skip tests as CP437 is not required by the spec.
      }

    byte[] utf8Bytes = asByteArray(new int[] { 0xC3,0x9F,0xE2,0x85,0x93,0xE2,0x82,0xAF,0xF0,0x90,0x85,0x80 });
    checkString(h, new String(utf8Bytes, Charset.forName("UTF8")),
		"\u00DF\u2153\u20AF\uD800\uDD40");

    byte[] isoBytes = asByteArray(new int[] {0x48,0x65,0x6C,0x6C,0x6F,0x20,0x57,0x6F,0x72,0x6C,0x64,0x21});
    checkString(h, new String(isoBytes, Charset.forName("ISO-8859-1")),
		"Hello World!");
  }

  private void checkString(TestHarness h, String result, String expected)
  {
    for (int a = 0; a < result.length(); ++a)
      {
	h.check(result.charAt(a), expected.charAt(a));
      }
    h.check(result, expected);
  }

  private byte[] asByteArray(int[] ints)
  {
    byte[] bytes = new byte[ints.length];
    for (int a = 0; a < ints.length; ++a)
      bytes[a] = Integer.valueOf(ints[a]).byteValue();
    return bytes;
  }
      
}
