// Tags: JDK1.0

// Copyright (C) 1998, 1999 Cygnus Solutions

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

package gnu.testlet.java.lang.Long;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class new_Long implements Testlet
{
  public void test (TestHarness harness)
    {
      Long a = new Long(0L);
      Long b = new Long(1L);
      Long c = new Long(-1L);
      Long d = new Long(Long.MAX_VALUE);
      Long e = new Long(Long.MIN_VALUE);
      Long f = new Long("0");
      Long g = new Long("1");
      Long h = new Long("-1");
      Long i = new Long("9223372036854775807");
      Long j = new Long("-9223372036854775808");
      Long k = new Long("-0");
      Long l = new Long("012345");
      Long m = new Long("0012345");

      harness.check (a + " " + b + " " + c + " " + d + " " + e,
		     "0 1 -1 9223372036854775807 -9223372036854775808");
      harness.check (f + " " + g + " " + h + " " + i + " " + j,
		     "0 1 -1 9223372036854775807 -9223372036854775808");
      harness.check (k + " " + l + " " + m,
		     "0 12345 12345");
      harness.check ((long) Integer.MAX_VALUE, 2147483647);
      harness.check ((long) Integer.MIN_VALUE, -2147483648);
      harness.check (Long.MAX_VALUE, 9223372036854775807L);
      harness.check (Long.MAX_VALUE, 9223372036854775807L);
      harness.check (Long.MAX_VALUE + 1, -9223372036854775808L);
      harness.check (Long.MAX_VALUE + 2, -9223372036854775807L);
      harness.check (Long.MIN_VALUE, -9223372036854775808L);
      harness.check (Long.MIN_VALUE - 1, 9223372036854775807L);
      harness.check (Long.MIN_VALUE - 2, 9223372036854775806L);
      harness.check (c.toString(), "-1");
      harness.check (e.toString(), "-9223372036854775808");
      harness.check (Long.toString(-1L, 2),
		     "-1");
      harness.check (Long.toString(Long.MIN_VALUE + 1, 2),
		     "-111111111111111111111111111111111111111111111111111111111111111");
      harness.check (Long.toString(Long.MIN_VALUE, 2),
		     "-1000000000000000000000000000000000000000000000000000000000000000");
      harness.check (Long.toString(Long.MAX_VALUE, 2),
		     "111111111111111111111111111111111111111111111111111111111111111");
      harness.check (Long.toString(-1L, 16),
		     "-1");
      harness.check (Long.toString(Long.MIN_VALUE + 1, 16),
		     "-7fffffffffffffff");
      harness.check (Long.toString(Long.MIN_VALUE, 16),
		     "-8000000000000000");
      harness.check (Long.toString(Long.MAX_VALUE, 16),
		     "7fffffffffffffff");
      harness.check (Long.toString(-1L, 36),
		     "-1");
      harness.check (Long.toString(Long.MIN_VALUE + 1, 36),
		     "-1y2p0ij32e8e7");
      harness.check (Long.toString(Long.MIN_VALUE, 36),
		     "-1y2p0ij32e8e8");
      harness.check (Long.toString(Long.MAX_VALUE, 36),
		     "1y2p0ij32e8e7");
      harness.check (Long.toString(12345, 1), "12345");
      harness.check (Long.toString(-12345, 1), "-12345");
      harness.check (Long.toString(12345, 37), "12345");
      harness.check (Long.toString(-12345, 37), "-12345");
      harness.check (Long.toString(12345, 0), "12345");
      harness.check (Long.toString(-12345, 0), "-12345");
      harness.check (Long.toString(12345, -1), "12345");
      harness.check (Long.toString(-12345, -1), "-12345");
      harness.check (Long.toString(12345, Character.MIN_RADIX - 1), "12345");
      harness.check (Long.toString(12345, Character.MAX_RADIX + 1), "12345");

      Long bad = null;
      try
	{
	  bad = new Long("9223372036854775808");
	}
      catch (NumberFormatException ex)
	{
	}
      harness.check (bad, null);

      bad = null;
      try
	{
	  bad = new Long("-9223372036854775809");
	}
      catch (NumberFormatException ex)
	{
	}
      harness.check (bad, null);

      bad = null;
      try
	{
	  bad = new Long("12345a");
	}
      catch (NumberFormatException ex)
	{
	}
      harness.check (bad, null);

      bad = null;
      try
	{
	  bad = new Long("-");
	}
      catch (NumberFormatException ex)
	{
	}
      harness.check (bad, null);

      bad = null;
      try
	{
	  bad = new Long("0x1e");
	}
      catch (NumberFormatException ex)
	{
	}
      harness.check (bad, null);

      harness.check (a.hashCode(), 0);
      harness.check (b.hashCode(), 1);
      harness.check (c.hashCode(), 0);
      harness.check (d.hashCode(), -2147483648);
      harness.check (e.hashCode(), -2147483648);

    // harness.check (a.compareTo(a));
    // harness.check (b.compareTo(c));
    // harness.check (c.compareTo(b));
    // harness.check (d.compareTo(e));
    // harness.check (e.compareTo(d));

      boolean ok = false;
      try
	{
	  Long.parseLong("");
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Long.parseLong(" ");
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Long.parseLong("0X1234");
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Long.parseLong("0xF0000000");
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Long.parseLong("-");
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Long.parseLong("#");
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Long.parseLong("-0x1234FF");
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      harness.checkPoint ("toBinaryString");
      harness.check (Long.toBinaryString(0L),
		     "0");
      harness.check (Long.toBinaryString(1L),
		     "1");
      harness.check (Long.toBinaryString(-1L),
		     "1111111111111111111111111111111111111111111111111111111111111111");
      harness.check (Long.toBinaryString(Long.MIN_VALUE),
		     "1000000000000000000000000000000000000000000000000000000000000000");
      harness.check (Long.toBinaryString(Long.MAX_VALUE),
		     "111111111111111111111111111111111111111111111111111111111111111");
      harness.check (Long.toBinaryString(Long.MIN_VALUE - 1),
		     "111111111111111111111111111111111111111111111111111111111111111");
      harness.check (Long.toBinaryString(Long.MAX_VALUE + 1),
		     "1000000000000000000000000000000000000000000000000000000000000000");

      harness.checkPoint ("toOctalString");
      harness.check (Long.toOctalString(0L),
		     "0");
      harness.check (Long.toOctalString(1L),
		     "1");
      harness.check (Long.toOctalString(-1L),
		     "1777777777777777777777");
      harness.check (Long.toOctalString(Long.MIN_VALUE),
		     "1000000000000000000000");
      harness.check (Long.toOctalString(Long.MAX_VALUE),
		     "777777777777777777777");
      harness.check (Long.toOctalString(Long.MIN_VALUE - 1),
		     "777777777777777777777");
      harness.check (Long.toOctalString(Long.MAX_VALUE + 1),
		     "1000000000000000000000");

      harness.checkPoint ("toHexString");
      harness.check (Long.toHexString(0L),
		     "0");
      harness.check (Long.toHexString(1L),
		     "1");
      harness.check (Long.toHexString(-1L),
		     "ffffffffffffffff");
      harness.check (Long.toHexString(Long.MIN_VALUE),
		     "8000000000000000");
      harness.check (Long.toHexString(Long.MAX_VALUE),
		     "7fffffffffffffff");
      harness.check (Long.toHexString(Long.MIN_VALUE - 1),
		     "7fffffffffffffff");
      harness.check (Long.toHexString(Long.MAX_VALUE + 1),
		     "8000000000000000");

      harness.checkPoint ("parseLong");
      harness.check (Long.parseLong("0012345", 8),
		     5349);
      harness.check (Long.parseLong("xyz", 36),
		     44027);
      harness.check (Long.parseLong("12345", 6),
		     1865);
      harness.check (Long.parseLong("abcdef", 16),
		     11259375);
      harness.check (Long.parseLong("-0012345", 8),
		     -5349);
      harness.check (Long.parseLong("-xyz", 36),
		     -44027);
      harness.check (Long.parseLong("-12345", 6),
		     -1865);
      harness.check (Long.parseLong("-abcdef", 16),
		     -11259375);
      harness.check (Long.parseLong("-8000000000000000", 16),
		     Long.MIN_VALUE);
      harness.check (Long.parseLong("7fffffffffffffff", 16),
		     Long.MAX_VALUE);

      ok = false;
      try
	{
	  Long.parseLong("0", 1);
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Long.parseLong("0", 37);
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Long.parseLong("8000000000000000", 16);
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Long.parseLong("-8000000000000001", 16);
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);
    }
}
