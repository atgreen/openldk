// Tags: JDK1.0

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Mark Wielaard (mark@klomp.org)
// Based on input from James Clark (jjc@jclark.com)

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

public class surrogate implements Testlet
{
  public void test (TestHarness harness)
    {
      try
	{
	  byte[] cs = {(byte)0xf0, (byte)0x90, (byte)0x8c, (byte)0x80};
	  
	  int ch = 0x10300;
	  char[] v = new char[2];
	  v[0] = surrogate1(ch);
	  v[1] = surrogate2(ch);
	  String str = new String(v);
	  byte[] bs = str.getBytes("UTF-8");
	  harness.check(bs.length, cs.length);
	  for (int i = 0; i < bs.length; i++)
	    harness.check(bs[i], cs[i]);
	}
      catch (java.io.UnsupportedEncodingException _)
	{
	  harness.check(false, "UTF-8 UnsupportedEncodingException");
	}
    }

  static public char surrogate1(int c) {
    return (char)(((c - 0x10000) >> 10) | 0xD800);
  }

  static public char surrogate2(int c) {
    return (char)(((c - 0x10000) & 0x3FF) | 0xDC00);
  }
}
