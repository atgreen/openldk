// Tags: JDK1.0

// Copyright (C) 1998 Cygnus Solutions

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

public class decode implements Testlet
{
  public void test (TestHarness harness)
    {
      char[] cstr = { 'a', 'b', 'c', '\t', 'A', 'B', 'C', ' ', '1', '2', '3' };

      byte[] bstr = new byte [cstr.length];
      for (int i = 0; i < cstr.length; ++i)
	bstr[i] = (byte) cstr[i];

      String a = new String(bstr);
      String a_utf8 = "";
      String b = new String(bstr, 3, 3);
      String b_utf8 = "";
      String c = "";
      String d = "";

      try
	{
	  a_utf8 = new String(bstr, "UTF-8");
	}
      catch (UnsupportedEncodingException ex)
	{
	}

      try
	{
	  b_utf8 = new String(bstr, 3, 3, "UTF-8");
	}
      catch (UnsupportedEncodingException ex)
	{
	}

      try
	{
	  c = new String(bstr, "8859_1");
	}
      catch (UnsupportedEncodingException ex)
	{
	}

      try
	{
	  d = new String(bstr, 3, 3, "8859_1");
	}
      catch (UnsupportedEncodingException ex)
	{
	}

      harness.check (a, "abc	ABC 123");
      harness.check (a_utf8, "abc	ABC 123");
      harness.check (b, "	AB");
      harness.check (b_utf8, "	AB");
      harness.check (c, "abc	ABC 123");
      harness.check (d, "	AB");

      boolean ok = false;
      try
	{
	  c = new String(bstr, "foobar8859_1");
	}
      catch (UnsupportedEncodingException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  d = new String(bstr, 3, 3, "foobar8859_1");
	}
      catch (UnsupportedEncodingException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      harness.check (String.copyValueOf(cstr), "abc	ABC 123");
      harness.check (String.copyValueOf(cstr, 3, 3), "	AB");

      byte[] leWithBOM = new byte[]
        {(byte)0xFF, (byte)0xFE, (byte)'a', (byte)0x00};
      byte[] leWithoutBOM = new byte[]
        {(byte)'a', (byte)0x00};
      byte[] beWithBOM = new byte[]
        {(byte)0xFE, (byte)0xFF, (byte)0x00, (byte)'a'};
      byte[] beWithoutBOM = new byte[]
        {(byte)0x00, (byte)'a'};

      // UTF-16: Big endian assumed without BOM
      harness.check(decodeTest(leWithBOM, "UTF-16", "a"));
      harness.check(!decodeTest(leWithoutBOM, "UTF-16", "a"));
      harness.check(decodeTest(beWithBOM, "UTF-16", "a"));
      harness.check(decodeTest(beWithoutBOM, "UTF-16", "a"));

      // UTF-16LE: BOM should not be used
      harness.check(!decodeTest(leWithBOM, "UTF-16LE", "a"));
      harness.check(decodeTest(leWithoutBOM, "UTF-16LE", "a"));
      harness.check(!decodeTest(beWithBOM, "UTF-16LE", "a"));
      harness.check(!decodeTest(beWithoutBOM, "UTF-16LE", "a"));

      // UTF-16BE: BOM should not be used
      harness.check(!decodeTest(leWithBOM, "UTF-16BE", "a"));
      harness.check(!decodeTest(leWithoutBOM, "UTF-16BE", "a"));
      harness.check(!decodeTest(beWithBOM, "UTF-16BE", "a"));
      harness.check(decodeTest(beWithoutBOM, "UTF-16BE", "a"));

      // UnicodeLittle: Little endian assumed without BOM
      harness.check(decodeTest(leWithBOM, "UnicodeLittle", "a"));
      harness.check(decodeTest(leWithoutBOM, "UnicodeLittle", "a"));
      harness.check(!decodeTest(beWithBOM, "UnicodeLittle", "a"));
      harness.check(!decodeTest(beWithoutBOM, "UnicodeLittle", "a"));
    }

  public boolean decodeTest (byte[] bytes, String encoding, String expected)
    {
      try
        {
          String s = new String(bytes, encoding);
          return s.equals(expected);
        }        
      catch (UnsupportedEncodingException ex)
	{
	  return false;
	}
    }

}
