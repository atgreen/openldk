// Tags: JDK1.0

// Copyright (C) 1998, 1999, 2001 Cygnus Solutions

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

package gnu.testlet.java.lang.Integer;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class new_Integer implements Testlet
{
  public void test (TestHarness harness)
    {
      Integer a = new Integer(0);
      Integer b = new Integer(1);
      Integer c = new Integer(-1);
      Integer d = new Integer(Integer.MAX_VALUE);
      Integer e = new Integer(Integer.MIN_VALUE);
      Integer f = new Integer("0");
      Integer g = new Integer("1");
      Integer h = new Integer("-1");
      Integer i = new Integer("2147483647");
      Integer j = new Integer("-2147483648");
      Integer k = new Integer("-0");
      Integer l = new Integer("012345");
      Integer m = new Integer("0012345");

      harness.checkPoint ("toString");
      harness.check (a + " " + b + " " + c + " " + d + " " + e,
		     "0 1 -1 2147483647 -2147483648");
      harness.check (f + " " + g + " " + h + " " + i + " " + j,
		     "0 1 -1 2147483647 -2147483648");
      harness.check (k + " " + l + " " + m,
		     "0 12345 12345");
      harness.check (Integer.MAX_VALUE, 2147483647);
      harness.check (Integer.MAX_VALUE + 1, -2147483648);
      harness.check (Integer.MAX_VALUE + 2, -2147483647);
      harness.check (Integer.MIN_VALUE, -2147483648);
      harness.check (Integer.MIN_VALUE - 1, 2147483647);
      harness.check (Integer.MIN_VALUE - 2, 2147483646);
      harness.check (c.toString(), "-1");
      harness.check (e.toString(), "-2147483648");
      harness.check (Integer.toString(-1, 2),
		     "-1");
      harness.check (Integer.toString(Integer.MIN_VALUE + 1, 2),
		     "-1111111111111111111111111111111");
      harness.check (Integer.toString(Integer.MIN_VALUE, 2),
		     "-10000000000000000000000000000000");
      harness.check (Integer.toString(Integer.MAX_VALUE, 2),
		     "1111111111111111111111111111111");
      harness.check (Integer.toString(-1, 16),
		     "-1");
      harness.check (Integer.toString(Integer.MIN_VALUE + 1, 16),
		     "-7fffffff");
      harness.check (Integer.toString(Integer.MIN_VALUE, 16),
		     "-80000000");
      harness.check (Integer.toString(Integer.MAX_VALUE, 16),
		     "7fffffff");
      harness.check (Integer.toString(-1, 36),
		     "-1");
      harness.check (Integer.toString(Integer.MIN_VALUE + 1, 36),
		     "-zik0zj");
      harness.check (Integer.toString(Integer.MIN_VALUE, 36),
		     "-zik0zk");
      harness.check (Integer.toString(Integer.MAX_VALUE, 36),
		     "zik0zj");
      harness.check (Integer.toString(12345, 1), "12345");
      harness.check (Integer.toString(-12345, 1), "-12345");
      harness.check (Integer.toString(12345, 37), "12345");
      harness.check (Integer.toString(-12345, 37), "-12345");
      harness.check (Integer.toString(12345, 0), "12345");
      harness.check (Integer.toString(-12345, 0), "-12345");
      harness.check (Integer.toString(12345, -1), "12345");
      harness.check (Integer.toString(-12345, -1), "-12345");
      harness.check (Integer.toString(12345, Character.MIN_RADIX - 1), "12345");
      harness.check (Integer.toString(12345, Character.MAX_RADIX + 1), "12345");

      harness.checkPoint ("exceptions");
      Integer bad = null;
      try
	{
	  bad = new Integer("2147483648");
	}
      catch (NumberFormatException ex)
	{
	}
      harness.check (bad, null);

      bad = null;
      try
	{
	  bad = new Integer("-2147483649");
	}
      catch (NumberFormatException ex)
	{
	}
      harness.check (bad, null);

      bad = null;
      try
	{
	  bad = new Integer("12345a");
	}
      catch (NumberFormatException ex)
	{
	}
      harness.check (bad, null);

      bad = null;
      try
	{
	  bad = new Integer("-");
	}
      catch (NumberFormatException ex)
	{
	}
      harness.check (bad, null);

      bad = null;
      try
	{
	  bad = new Integer("0x1e");
	}
      catch (NumberFormatException ex)
	{
	}
      harness.check (bad, null);

      bad = null;
      try
	{
	  bad = new Integer(null);
	}
      catch (NullPointerException npe)
        {
	  harness.fail("wrong exception");
	}
      catch (NumberFormatException ex)
	{
	}
      harness.check (bad, null);

      bad = null;
      try
	{
	  bad = new Integer(" ");
	}
      catch (NumberFormatException ex)
	{
	}
      harness.check (bad, null);

      harness.checkPoint ("hashCode");
      harness.check (a.hashCode(), 0);
      harness.check (b.hashCode(), 1);
      harness.check (c.hashCode(), -1);
      harness.check (d.hashCode(), 2147483647);
      harness.check (e.hashCode(), -2147483648);

    // harness.check (a.compareTo(a));
    // harness.check (b.compareTo(c));
    // harness.check (c.compareTo(b));
    // harness.check (d.compareTo(e));
    // harness.check (e.compareTo(d));

      harness.checkPoint ("decode");
      harness.check (Integer.decode("123456789"), new Integer (123456789));
      harness.check (Integer.decode("01234567"), new Integer (342391));
      harness.check (Integer.decode("0x1234FF"), new Integer (1193215));
      harness.check (Integer.decode("#1234FF"), new Integer (1193215));
      harness.check (Integer.decode("-123456789"), new Integer (-123456789));
      harness.check (Integer.decode("-01234567"), new Integer (-01234567));
      harness.check (Integer.decode("-0"), new Integer (0));
      harness.check (Integer.decode("0"), new Integer (0));
      harness.check (Integer.decode(Integer.toString(Integer.MIN_VALUE)),
		     new Integer (-2147483648));
      harness.check (Integer.decode("-01"), new Integer(-1));
      harness.check (Integer.decode("-0x1"), new Integer(-1));
      harness.check (Integer.decode("-#1"), new Integer(-1));
      // \\u0660 is a Unicode digit, value 0, but does not trigger octal or hex
      harness.check (Integer.decode("\u06609"), new Integer(9));

      harness.checkPoint ("decode exceptions");
      boolean ok = false;
      try
	{
	  Integer.decode("");
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Integer.decode(" ");
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Integer.decode(null);
	}
      catch (NullPointerException npe)
	{
	  ok = true;
	}
      catch (NumberFormatException ex)
	{
	  // not ok
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Integer.decode("X1234");
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Integer.decode("0xF0000000");
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Integer.decode("0x");
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Integer.decode("-");
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Integer.decode("#");
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      harness.checkPoint ("toBinaryString");
      harness.check (Integer.toBinaryString(0),
		     "0");
      harness.check (Integer.toBinaryString(1),
		     "1");
      harness.check (Integer.toBinaryString(-1),
		     "11111111111111111111111111111111");
      harness.check (Integer.toBinaryString(Integer.MIN_VALUE),
		     "10000000000000000000000000000000");
      harness.check (Integer.toBinaryString(Integer.MAX_VALUE),
		     "1111111111111111111111111111111");
      harness.check (Integer.toBinaryString(Integer.MIN_VALUE - 1),
		     "1111111111111111111111111111111");
      harness.check (Integer.toBinaryString(Integer.MAX_VALUE + 1),
		     "10000000000000000000000000000000");

      harness.checkPoint ("toOctalString");
      harness.check (Integer.toOctalString(0),
		     "0");
      harness.check (Integer.toOctalString(1),
		     "1");
      harness.check (Integer.toOctalString(-1),
		     "37777777777");
      harness.check (Integer.toOctalString(Integer.MIN_VALUE),
		     "20000000000");
      harness.check (Integer.toOctalString(Integer.MAX_VALUE),
		     "17777777777");
      harness.check (Integer.toOctalString(Integer.MIN_VALUE - 1),
		     "17777777777");
      harness.check (Integer.toOctalString(Integer.MAX_VALUE + 1),
		     "20000000000");

      harness.checkPoint ("toHexString");
      harness.check (Integer.toHexString(0),
		     "0");
      harness.check (Integer.toHexString(1),
		     "1");
      harness.check (Integer.toHexString(-1),
		     "ffffffff");
      harness.check (Integer.toHexString(Integer.MIN_VALUE),
		     "80000000");
      harness.check (Integer.toHexString(Integer.MAX_VALUE),
		     "7fffffff");
      harness.check (Integer.toHexString(Integer.MIN_VALUE - 1),
		     "7fffffff");
      harness.check (Integer.toHexString(Integer.MAX_VALUE + 1),
		     "80000000");

      harness.checkPoint ("parseInt");
      harness.check (Integer.parseInt("0012345", 8),
		     5349);
      harness.check (Integer.parseInt("xyz", 36),
		     44027);
      harness.check (Integer.parseInt("12345", 6),
		     1865);
      harness.check (Integer.parseInt("abcdef", 16),
		     11259375);
      harness.check (Integer.parseInt("-0012345", 8),
		     -5349);
      harness.check (Integer.parseInt("-xyz", 36),
		     -44027);
      harness.check (Integer.parseInt("-12345", 6),
		     -1865);
      harness.check (Integer.parseInt("-abcdef", 16),
		     -11259375);
      harness.check (Integer.parseInt("0", 25),
		     0);

      ok = false;
      try
	{
	  Integer.parseInt("0", 1);
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Integer.parseInt("0", 37);
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Integer.parseInt ("-80000001", 16);
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  // This should fail, but won't if you are using a naive
	  // overflow detection scheme.  `429496730' is chosen because
	  // when multiplied by 10 it overflows but the result is
	  // positive.
	  Integer.parseInt ("4294967309", 10);
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);

      ok = false;
      try
	{
	  Integer.parseInt ("800000001", 16);
	}
      catch (NumberFormatException ex)
	{
	  ok = true;
	}
      harness.check (ok);
    }
}
