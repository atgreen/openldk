// Tags: JDK1.3

// Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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
import java.io.UnsupportedEncodingException;

public class getBytes13 implements Testlet
{
  protected static final byte[] ABC1 = new byte[] {97, 98, 99};
  protected static final byte[] ABC2 = new byte[] {-2, -1,  0, 97,  0, 98,  0, 99};
  protected static final byte[] ABC3 = new byte[] { 0, 97,  0, 98,  0, 99};
  protected static final byte[] ABC4 = new byte[] {-1, -2, 97,  0, 98,  0, 99,  0};
  protected static final byte[] ABC5 = new byte[] {97,  0, 98,  0, 99,  0};

  public void test (TestHarness harness)
  {
    harness.checkPoint("getBytes13");

    test1Encoding (harness, "ASCII",                 "abc", ABC1);
    test1Encoding (harness, "Cp1252",                "abc", ABC1);
    test1Encoding (harness, "ISO8859_1",             "abc", ABC1);
    test1Encoding (harness, "UTF8",                  "abc", ABC1);
    test1Encoding (harness, "UTF-16",                "abc", ABC2);
    test1Encoding (harness, "UnicodeBig",            "abc", ABC2);
    test1Encoding (harness, "UnicodeBigUnmarked",    "abc", ABC3);
    test1Encoding (harness, "UnicodeLittle",         "abc", ABC4);
    test1Encoding (harness, "UnicodeLittleUnmarked", "abc", ABC5);
  }


  protected void
  test1Encoding (TestHarness h, String encoding, String s, byte[] ba)
  {
    String signature = "String.getBytes(\""+encoding+"\")";
    try
      {
	byte[] theBytes = s.getBytes(encoding);
	boolean result = areEqual(theBytes, ba);
        h.check (result, signature);
	if (! result)
	  {
	    dumpArray(h, "Got     : ", theBytes);
	    dumpArray(h, "Expected: ", ba);
	  }
      }
    catch (UnsupportedEncodingException x)
      {
        h.debug (x);
	h.fail (signature);
      }
  }

  static void dumpArray(TestHarness h, String prefix, byte[] a)
  {
    StringBuffer result = new StringBuffer(prefix);
    for (int i = 0; i < a.length; ++i)
      {
	if (i > 0)
	  result.append(' ');
	result.append(a[i]);
      }
    h.debug(result.toString());
  }

  static boolean areEqual (byte[] a, byte[] b)
  {
    if (a == null || b == null)
      return false;
    if (a.length != b.length)
      return false;
    for (int i = 0; i < a.length; i++)
      if (a[i] != b[i])
	return false;
    return true;
  }
}
